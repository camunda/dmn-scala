package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{FunctionDefinition, FormalParameter, Expression, LiteralExpression}
import org.camunda.feel.{FeelEngine, EvalValue}
import org.camunda.feel.interpreter.{ValFunction, ValError, DefaultValueMapper}

class FunctionDefinitionEvaluator(feelEngine: FeelEngine) {
  
  def eval(function: FunctionDefinition, context: EvalContext): Either[Failure, Any] = 
  {
    val parameters = function.getFormalParameters.asScala
    val expression = function.getExpression
    
    val parameterNames = parameters.map(_.getName).toList
    
    expression match {
      case lt: LiteralExpression => Right(createFunction(lt, parameterNames, context))
      case other                 => Left(Failure(s"exprected literal expression but found '$other'")) 
    }
  }
  
  private def createFunction(literalExpression: LiteralExpression, parameterNames: List[String], context: EvalContext): ValFunction = 
  {
    val expr = literalExpression.getText.getTextContent
    val parsedExpression = context.parsedExpressions(expr)
    
    ValFunction(
      params = parameterNames,
      invoke = args => 
        {
          // TODO check type of parameters
          
          val arguments = parameterNames.zip(args)
          
          feelEngine.eval(parsedExpression, context.variables ++ arguments) match {
            case EvalValue(value) => 
            {
              // TODO avoid unpack and re-pack of result
              DefaultValueMapper.instance.toVal(value)
            }
            case e => ValError(e.toString)
          }
        } 
    )
  }
  
}