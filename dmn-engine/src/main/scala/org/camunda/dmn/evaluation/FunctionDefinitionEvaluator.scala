package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{FunctionDefinition, FormalParameter, Expression, LiteralExpression}
import org.camunda.feel.interpreter.{ValFunction, ValError, DefaultValueMapper}

class FunctionDefinitionEvaluator(eval: (LiteralExpression, EvalContext) => Either[Failure, Any]) {
  
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
    ValFunction(
      params = parameterNames,
      invoke = args => 
        {
          // TODO check type of parameters
          val arguments = parameterNames.zip(args)
          
          eval(literalExpression, context.copy(variables = context.variables ++ arguments)) match {
            case Right(value) => DefaultValueMapper.instance.toVal(value)
            case Left(error)  => ValError(error.toString)
          }
        } 
    )
  }
  
}