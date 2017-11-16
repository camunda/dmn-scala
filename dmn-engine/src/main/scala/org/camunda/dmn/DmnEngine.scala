package org.camunda.dmn

import scala.collection.JavaConverters._

import java.io.InputStream

import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{Decision, DecisionTable}
import org.camunda.feel._
import org.camunda.feel.FeelEngine
import org.camunda.feel.ParsedExpression

object DmnEngine {
  
  case class Failure(message: String)
  
  sealed trait EvalResult
  
  case class Result(value: Any) extends EvalResult
  
  case object NilResult extends EvalResult
  
  case class EvalContext(
    variables: Map[String, Any],
    parsedExpressions: Map[String, ParsedExpression]
  )
  
}

class DmnEngine {
  
  import DmnEngine._
  
  val parser = new DmnParser

  val feelEngine = new FeelEngine
  
  val decisionTableProcessor = new DecisionTableProcessor(feelEngine)
  
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

  private def evalDecision(decision: Decision, context: EvalContext): Either[Failure, EvalResult] = {

    val decisionId = decision.getId
    val decisionName = decision.getName

    val variable = Option.apply(decision.getVariable)
    val variableName = variable.map(_.getName).getOrElse(decisionId)

    val expression = decision.getExpression

    if (expression.isInstanceOf[DecisionTable]) {

      val decisionTable = expression.asInstanceOf[DecisionTable]

      decisionTableProcessor.eval(decisionTable)(context)
        .right
        .map(result => result
          .map(Result)
          .getOrElse(NilResult))
        
    } else {
      Left(Failure(s"decision of type '${expression.getTypeRef}' is not supported"))
    }
  }

    
}