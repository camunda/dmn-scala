package org.camunda.dmn

import scala.collection.JavaConverters._
import scala.reflect.{ ClassTag, classTag }

import java.io.InputStream
import java.util.ServiceLoader

import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser._
import org.camunda.dmn.evaluation._
import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{ Decision, DecisionTable, Expression, BusinessKnowledgeModel, LiteralExpression }
import org.camunda.bpm.model.dmn.instance.{ Invocation, Binding, FormalParameter, Context }
import org.camunda.bpm.model.dmn.instance.{ List => DmnList, Relation, FunctionDefinition }
import org.camunda.feel._
import org.camunda.feel.{ FeelEngine, ParsedExpression }
import org.camunda.feel.interpreter.{ Val, ValNull, FunctionProvider, ValueMapper, DefaultValueMapper }
import org.camunda.feel.spi.{ CustomValueMapper, CustomFunctionProvider }

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

  case class EvalContext(
    dmn:       ParsedDmn,
    variables: Map[String, Any])

}

class DmnEngine {

  import DmnEngine._

  val valueMapper = loadValueMapper()

  val feelEngine = new FeelEngine(
    functionProvider = loadFunctionProvider(),
    valueMapper = new NoUnpackValueMapper(valueMapper))

  val parser = new DmnParser

  val decisionEval = new DecisionEvaluator(
    eval = this.evalExpression,
    evalBkm = bkmEval.createFunction)

  val literalExpressionEval = new LiteralExpressionEvaluator(feelEngine)

  val decisionTableEval = new DecisionTableEvaluator(literalExpressionEval.evalExpression)

  val bkmEval = new BusinessKnowledgeEvaluator(this.evalExpression, valueMapper)

  val contextEval = new ContextEvaluator(this.evalExpression)

  val listEval = new ListEvaluator(this.evalExpression)

  val relationEval = new RelationEvaluator(this.evalExpression)

  val invocationEval = new InvocationEvaluator(
    eval = literalExpressionEval.evalExpression,
    evalBkm = bkmEval.eval)

  val functionDefinitionEval = new FunctionDefinitionEvaluator(literalExpressionEval.evalExpression)

  ///// public API

  def eval(stream: InputStream, decisionId: String, context: Map[String, Any]): Either[Failure, EvalResult] = {
    parse(stream).right.flatMap(parsedDmn => eval(parsedDmn, decisionId, context))
  }

  def parse(stream: InputStream): Either[Failure, ParsedDmn] = parser.parse(stream)

  def eval(dmn: ParsedDmn, decisionId: String, variables: Map[String, Any]): Either[Failure, EvalResult] = {
    dmn.decisionsById.get(decisionId)
      .map(evalDecision(_, EvalContext(dmn, variables)))
      .getOrElse(Left(Failure(s"no decision found with id '$decisionId'")))
  }

  def evalByName(dmn: ParsedDmn, decisionName: String, variables: Map[String, Any]): Either[Failure, EvalResult] = {
    dmn.decisionsByName.get(decisionName)
      .map(evalDecision(_, EvalContext(dmn, variables)))
      .getOrElse(Left(Failure(s"no decision found with name '$decisionName'")))
  }

  ///// Java public API

  def eval(stream: InputStream, decisionId: String, context: java.util.Map[String, Object]): Either[Failure, EvalResult] =
    eval(stream, decisionId, context.asScala.toMap)

  def eval(dmn: ParsedDmn, decisionId: String, variables: java.util.Map[String, Object]): Either[Failure, EvalResult] =
    eval(dmn, decisionId, variables.asScala.toMap)

  def evalByName(dmn: ParsedDmn, decisionName: String, variables: java.util.Map[String, Object]): Either[Failure, EvalResult] =
    evalByName(dmn, decisionName, variables.asScala.toMap)

  ///// internal

  private def evalDecision(decision: ParsedDecision, context: EvalContext): Either[Failure, EvalResult] = {
    decisionEval.eval(decision, context)
      .right
      .map(
        {
          case ValNull => NilResult
          case result  => Result(valueMapper.unpackVal(result))
        })
  }

  private def evalExpression(expression: ParsedDecisionLogic, context: EvalContext): Either[Failure, Val] = {
    expression match {
      case dt: ParsedDecisionTable     => decisionTableEval.eval(dt, context)
      case inv: ParsedInvocation       => invocationEval.eval(inv, context)
      case le: ParsedLiteralExpression => literalExpressionEval.evalExpression(le, context)
      case c: ParsedContext            => contextEval.eval(c, context)
      case l: ParsedList               => listEval.eval(l, context)
      case rel: ParsedRelation         => relationEval.eval(rel, context)
      case f: ParsedFunctionDefinition => functionDefinitionEval.eval(f, context)
      case _                           => Left(Failure(s"expression of type '$expression' is not supported"))
    }
  }

  private def loadValueMapper(): ValueMapper = {
    loadServiceProvider[CustomValueMapper]() match {
      case Nil => DefaultValueMapper.instance
      case m :: Nil =>
        {
          logger.info("Found custom value mapper: {}", m)
          m
        }
      case mappers =>
        {
          logger.warn("Found more than one custom value mapper: {}. Use the first one.", mappers)
          mappers.head
        }
    }
  }

  private def loadFunctionProvider(): FunctionProvider = {
    loadServiceProvider[CustomFunctionProvider]() match {
      case Nil => FunctionProvider.EmptyFunctionProvider
      case f :: Nil =>
        {
          logger.info("Found custom function provider: {}", f)
          f
        }
      case fs =>
        {
          logger.info("Found custom function providers: {}", fs)
          new FunctionProvider.CompositeFunctionProvider(fs)
        }
    }
  }

  private def loadServiceProvider[T: ClassTag](): List[T] = {
    try {
      val runtimeClass = classTag[T].runtimeClass.asInstanceOf[Class[T]]
      val loader = ServiceLoader.load(runtimeClass)

      loader.iterator.asScala.toList

    } catch {
      case t: Throwable =>
        {
          logger.error(s"Failed to load service provider: ${classTag[T].runtimeClass.getSimpleName}", t)
          List.empty
        }
    }
  }

}
