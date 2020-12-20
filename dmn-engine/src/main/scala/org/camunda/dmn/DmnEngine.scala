package org.camunda.dmn

import java.io.InputStream
import java.util.ServiceLoader
import org.camunda.dmn.Audit.{
  AuditLog,
  AuditLogEntry,
  AuditLogListener,
  ContextEvaluationResult,
  EvaluationResult
}
import org.camunda.dmn.evaluation._
import org.camunda.dmn.parser._
import org.camunda.feel.FeelEngine
import org.camunda.feel.context.{CustomFunctionProvider, FunctionProvider}
import org.camunda.feel.syntaxtree.{Val, ValError, ValNull}
import org.camunda.feel.valuemapper.{CustomValueMapper, ValueMapper}

import scala.collection.JavaConverters._
import scala.collection.mutable.{ListBuffer => mutableList}
import scala.reflect.{ClassTag, classTag}

object DmnEngine {

  case class Failure(message: String)

  sealed trait EvalResult {
    val isNil: Boolean
  }

  case class Result(value: Any) extends EvalResult {
    val isNil = false
  }

  case object NilResult extends EvalResult {
    val isNil = true
  }

  case class EvalContext(dmn: ParsedDmn,
                         variables: Map[String, Any],
                         currentElement: ParsedDecisionLogicContainer,
                         auditLog: mutableList[AuditLogEntry] =
                           mutableList.empty) {

    def audit(decisionLogic: ParsedDecisionLogic, result: EvaluationResult) {
      if (decisionLogic == currentElement.logic) {
        auditLog += AuditLogEntry(id = currentElement.id,
                                  name = currentElement.name,
                                  decisionLogic = decisionLogic,
                                  result = result)
      }
    }

    def audit(decisionLogic: ParsedDecisionLogic,
              result: Either[Failure, Val],
              resultFactory: Val => EvaluationResult  = SingleEvaluationResult) {
      result match {
        case Right(result) =>
          audit(decisionLogic, resultFactory(result))
        case Left(failure) =>
          audit(decisionLogic, resultFactory(ValError(failure.message)))
      }
      result
    }

  }

  case class Configuration(escapeNamesWithSpaces: Boolean = false,
                           escapeNamesWithDashes: Boolean = false)

  class Builder {

    private var escapeNamesWithSpaces_ = false
    private var escapeNamesWithDashes_ = false
    private var auditLogListeners_ = List[AuditLogListener]().toBuffer

    def escapeNamesWithSpaces(enabled: Boolean): Builder = {
      escapeNamesWithSpaces_ = enabled
      this
    }

    def escapeNamesWithDashes(enabled: Boolean): Builder = {
      escapeNamesWithDashes_ = enabled
      this
    }

    def addAuditListener(listener: AuditLogListener): Builder = {
      auditLogListeners_ += listener
      this
    }

    def build: DmnEngine =
      new DmnEngine(
        configuration = DmnEngine.Configuration(escapeNamesWithSpaces =
                                                  escapeNamesWithSpaces_,
                                                escapeNamesWithDashes =
                                                  escapeNamesWithDashes_),
        auditLogListeners = auditLogListeners_.toList
      )

  }

}

class DmnEngine(configuration: DmnEngine.Configuration =
                  DmnEngine.Configuration(),
                auditLogListeners: List[AuditLogListener] = List.empty) {

  import DmnEngine._

  val valueMapper = loadValueMapper()

  val feelEngine = new FeelEngine(
    functionProvider = loadFunctionProvider(),
    valueMapper = ValueMapper.CompositeValueMapper(
      List(new NoUnpackValueMapper(valueMapper))))

  val parser = new DmnParser(
    configuration = configuration,
    parser = exp =>
      feelEngine
        .parseExpression(exp)
        .left
        .map(_.message)
  )

  val decisionEval = new DecisionEvaluator(eval = this.evalExpression,
                                           evalBkm = bkmEval.createFunction)

  val literalExpressionEval = new LiteralExpressionEvaluator(feelEngine)

  val decisionTableEval = new DecisionTableEvaluator(
    literalExpressionEval.evalExpression)

  val bkmEval = new BusinessKnowledgeEvaluator(this.evalExpression, valueMapper)

  val contextEval = new ContextEvaluator(this.evalExpression)

  val listEval = new ListEvaluator(this.evalExpression)

  val relationEval = new RelationEvaluator(this.evalExpression)

  val invocationEval = new InvocationEvaluator(
    eval = literalExpressionEval.evalExpression,
    evalBkm = bkmEval.eval)

  val functionDefinitionEval = new FunctionDefinitionEvaluator(
    literalExpressionEval.evalExpression)

  ///// public API

  def eval(stream: InputStream,
           decisionId: String,
           context: Map[String, Any]): Either[Failure, EvalResult] = {
    parse(stream).flatMap(parsedDmn => eval(parsedDmn, decisionId, context))
  }

  def parse(stream: InputStream): Either[Failure, ParsedDmn] =
    parser.parse(stream)

  def eval(dmn: ParsedDmn,
           decisionId: String,
           variables: Map[String, Any]): Either[Failure, EvalResult] = {
    dmn.decisionsById
      .get(decisionId)
      .map(decision =>
        evalDecision(decision, EvalContext(dmn, variables, decision)))
      .getOrElse(Left(Failure(s"no decision found with id '$decisionId'")))
  }

  def evalByName(dmn: ParsedDmn,
                 decisionName: String,
                 variables: Map[String, Any]): Either[Failure, EvalResult] = {
    dmn.decisionsByName
      .get(decisionName)
      .map(decision =>
        evalDecision(decision, EvalContext(dmn, variables, decision)))
      .getOrElse(Left(Failure(s"no decision found with name '$decisionName'")))
  }

  ///// Java public API

  def eval(
      stream: InputStream,
      decisionId: String,
      context: java.util.Map[String, Object]): Either[Failure, EvalResult] =
    eval(stream, decisionId, context.asScala.toMap)

  def eval(
      dmn: ParsedDmn,
      decisionId: String,
      variables: java.util.Map[String, Object]): Either[Failure, EvalResult] =
    eval(dmn, decisionId, variables.asScala.toMap)

  def evalByName(
      dmn: ParsedDmn,
      decisionName: String,
      variables: java.util.Map[String, Object]): Either[Failure, EvalResult] =
    evalByName(dmn, decisionName, variables.asScala.toMap)

  ///// internal

  private def evalDecision(
      decision: ParsedDecision,
      context: EvalContext): Either[Failure, EvalResult] = {
    decisionEval
      .eval(decision, context)
      .map {
        case ValNull => NilResult
        case result  => Result(valueMapper.unpackVal(result))
      } match {
      case anyResult =>
        val log = AuditLog(context.dmn, context.auditLog.toList)
        auditLogListeners.foreach(_.onEval(log))
        anyResult
    }
  }

  private def evalExpression(expression: ParsedDecisionLogic,
                             context: EvalContext): Either[Failure, Val] = {
    expression match {
      case dt: ParsedDecisionTable => decisionTableEval.eval(dt, context)
      case inv: ParsedInvocation   => invocationEval.eval(inv, context)
      case le: ParsedLiteralExpression =>
        literalExpressionEval.evalExpression(le, context)
      case c: ParsedContext    => contextEval.eval(c, context)
      case l: ParsedList       => listEval.eval(l, context)
      case rel: ParsedRelation => relationEval.eval(rel, context)
      case f: ParsedFunctionDefinition =>
        functionDefinitionEval.eval(f, context)
      case _ =>
        Left(Failure(s"expression of type '$expression' is not supported"))
    }
  }

  private def loadValueMapper(): ValueMapper = {
    loadServiceProvider[CustomValueMapper]() match {
      case Nil => FeelEngine.defaultValueMapper
      case m :: Nil => {
        ValueMapper.CompositeValueMapper(List(m))
      }
      case mappers => {
        ValueMapper.CompositeValueMapper(mappers)
      }
    }
  }

  private def loadFunctionProvider(): FunctionProvider = {
    loadServiceProvider[CustomFunctionProvider]() match {
      case Nil => FunctionProvider.EmptyFunctionProvider
      case f :: Nil => {
        FunctionProvider.CompositeFunctionProvider(List(f))
      }
      case fs => {
        FunctionProvider.CompositeFunctionProvider(fs)
      }
    }
  }

  private def loadServiceProvider[T: ClassTag](): List[T] = {
    try {
      val runtimeClass = classTag[T].runtimeClass.asInstanceOf[Class[T]]
      val loader = ServiceLoader.load(runtimeClass)

      loader.iterator.asScala.toList

    } catch {
      case t: Throwable => {
        logger.error(
          s"Failed to load service provider: ${classTag[T].runtimeClass.getSimpleName}",
          t)
        List.empty
      }
    }
  }

}
