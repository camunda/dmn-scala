package org.camunda.dmn

import scala.collection.JavaConverters._

import java.io.InputStream

import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser._
import org.camunda.dmn.evaluation._
import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{Decision, DecisionTable, Expression, BusinessKnowledgeModel, LiteralExpression}
import org.camunda.bpm.model.dmn.instance.{Invocation, Binding, FormalParameter, Context}
import org.camunda.bpm.model.dmn.instance.{List => DmnList, Relation}
import org.camunda.feel._
import org.camunda.feel.{FeelEngine, ParsedExpression}
import org.camunda.feel.interpreter.ValNull
import org.camunda.dmn.parser.ParsedDmn

object DmnEngine {
  
  case class Failure(message: String)
  
  sealed trait EvalResult
  
  case class Result(value: Any) extends EvalResult
  
  case object NilResult extends EvalResult
  
  case class EvalContext(
    variables: Map[String, Any],
    parsedExpressions: Map[String, ParsedExpression],
    bkms: Map[String, BusinessKnowledgeModel] = Map.empty
  )
  
}

class DmnEngine {
  
  import DmnEngine._
  
  val parser = new DmnParser

  val feelEngine = new FeelEngine
  
  val decisionEval = new DecisionEvaluator(this.evalExpression)
  
  val bkmEval = new BusinessKnowledgeEvaluator(this.evalExpression)

  val literalExpressionEval = new LiteralExpressionEvaluator(feelEngine)
  
  val decisionTableEval = new DecisionTableEvaluator(
    eval = literalExpressionEval.evalExpression, 
    unaryTests = literalExpressionEval.evalUnaryTests)
  
  val contextEval = new ContextEvaluator(this.evalExpression)
  
  val listEval = new ListEvaluator(this.evalExpression)
  
  val relationEval = new RelationEvaluator(this.evalExpression)
  
  val invocationEval = new InvocationEvaluator(
    evalExpression = this.evalExpression,
    evalBkm = bkmEval.eval)
  
  
  def eval(stream: InputStream, decisionId: String, context: Map[String, Any]): Either[Failure, EvalResult] = {
    parse(stream).right.flatMap( parsedDmn => eval(parsedDmn, decisionId, context))
  }
  
  def parse(stream: InputStream): Either[Failure, ParsedDmn] = parser.parse(stream)
  
  def eval(dmn: ParsedDmn, decisionId: String, variables: Map[String, Any]): Either[Failure, EvalResult] = {
    findDecision(dmn.model, decisionId)
          .map(evalDecision(_, EvalContext(variables, dmn.expressionsById)))
          .getOrElse(Left(Failure(s"no decision found with id '$decisionId'")))
  }
  
  private def findDecision(model: DmnModelInstance, decisionId: String): Option[Decision] = {
    model.getDefinitions
      .getDrgElements.asScala
      .find(e => e.isInstanceOf[Decision] && e.getId == decisionId)
      .map(_.asInstanceOf[Decision])
  }

  private def evalDecision(decision: Decision, context: EvalContext): Either[Failure, EvalResult] = 
  {
    decisionEval.eval(decision, context)
      .right
      .map(
      {
        case ValNull => NilResult
        case r       => Result(r)
      })
  }
  
  private def evalExpression(expression: Expression, context: EvalContext): Either[Failure, Any] = 
  {
    expression match {
      case dt: DecisionTable     => decisionTableEval.eval(dt, context)
      case inv: Invocation       => invocationEval.eval(inv, context)
      case le: LiteralExpression => literalExpressionEval.evalExpression(le, context)
      case c: Context            => contextEval.eval(c, context)
      case l: DmnList            => listEval.eval(l, context)
      case rel: Relation         => relationEval.eval(rel, context)
      case _                     => Left(Failure(s"expression of type '${expression.getTypeRef}' is not supported"))
    }
  }
    
}