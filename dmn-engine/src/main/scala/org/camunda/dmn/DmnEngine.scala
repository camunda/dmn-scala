package org.camunda.dmn

import scala.collection.JavaConverters._

import java.io.InputStream

import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{Decision, DecisionTable, Expression, BusinessKnowledgeModel, LiteralExpression}
import org.camunda.bpm.model.dmn.instance.{Invocation, Binding, FormalParameter}
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
    parsedExpressions: Map[String, ParsedExpression],
    bkms: Map[String, BusinessKnowledgeModel] = Map.empty
  )
  
}

class DmnEngine {
  
  import DmnEngine._
  
  val parser = new DmnParser

  val feelEngine = new FeelEngine
  
  val decisionTableProcessor = new DecisionTableProcessor(feelEngine)
  val literalExpressionProcessor = new LiteralExpressionProcessor(feelEngine)
  
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

    val knowledgeRequirements = decision.getKnowledgeRequirements.asScala
    
    val requiredBkms = knowledgeRequirements.map(kr => {
      val bkm = kr.getRequiredKnowledge
      bkm.getName -> bkm
    })
    .toMap
            
    val expression = decision.getExpression

    evalExpression(expression, EvalContext(context.variables, context.parsedExpressions, context.bkms ++ requiredBkms))
      .right
      .map(result => result
        .map(Result)
        .getOrElse(NilResult))
  }
  
  private def evalExpression(expression: Expression, context: EvalContext): Either[Failure, Option[Any]] = 
  {
    expression match {
      case dt: DecisionTable => decisionTableProcessor.eval(dt)(context)
      case inv: Invocation   => evalInvocation(inv, context)
      case le: LiteralExpression => (literalExpressionProcessor.eval(le)(context)).right.map(Some(_))
      case _                 => Left(Failure(s"expression of type '${expression.getTypeRef}' is not supported"))
    }
  }
  
  private def evalInvocation(invocation: Invocation, context: EvalContext): Either[Failure, Option[Any]] = {
    
    val bindings = invocation.getBindings.asScala
    
    val parameters = mapEither(bindings, (binding: Binding) => {
      
      // ---> bug in model API!!!
      val paramName = binding.getParameter.getName
      
      evalExpression(binding.getExpression, context)
        .right
        .map(value => paramName -> value.get)
    })
    
    parameters
      .right
      .flatMap(p => {
          
          val ctx = EvalContext(context.variables ++ p.toMap, context.parsedExpressions, context.bkms)
        
          invocation.getExpression match {
              case le: LiteralExpression => {
                  
                  val bkmName = le.getText.getTextContent
                          
                  context.bkms.get(bkmName)
                    .map(bkm => evalBusinessKnowledgeModel(bkm, ctx))
                    .getOrElse(Left(Failure(s"no BKM found with name '$bkmName'")))
              }
              case other => Left(Failure(s"expected invocation with literal expression but found '$other'"))
          }
      })
  }
  
  private def evalBusinessKnowledgeModel(bkm: BusinessKnowledgeModel, context: EvalContext): Either[Failure, Option[Any]] = {
    
    val logic = bkm.getEncapsulatedLogic
    val expression = logic.getExpression
    
    val parameters = logic.getFormalParameters.asScala
    
    // TODO check type of parameters
    val parameterValidation = mapEither(parameters, (p: FormalParameter) => 
      context.variables.get(p.getName)
        .map(Right(_))
        .getOrElse(Left(Failure(s"no parameter found with name '${p.getName}'")))
    )
    
    parameterValidation
      .right
      .flatMap(_ => evalExpression(expression, context))
  }

    
}