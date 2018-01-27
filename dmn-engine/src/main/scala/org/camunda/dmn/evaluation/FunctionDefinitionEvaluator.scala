package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{FunctionDefinition, FormalParameter, Expression, LiteralExpression}
import org.camunda.feel.interpreter.{ValFunction, ValError, DefaultValueMapper, Val}

class FunctionDefinitionEvaluator(eval: (LiteralExpression, EvalContext) => Either[Failure, Any]) {
  
  def eval(function: FunctionDefinition, context: EvalContext): Either[Failure, Any] = 
  {
    val parameters = function.getFormalParameters.asScala
    val expression = function.getExpression
    
    expression match {
      case lt: LiteralExpression => Right(createFunction(lt, parameters, context))
      case other                 => Left(Failure(s"exprected literal expression but found '$other'")) 
    }
  }
  
  private def createFunction(literalExpression: LiteralExpression, parameters: Iterable[FormalParameter], context: EvalContext): ValFunction = 
  {
    val parameterWithTypes = parameters.map(p => p.getName -> p.getTypeRef).toMap
    val parameterNames = parameterWithTypes.keys.toList
    
    ValFunction(
      params = parameterNames,
      invoke = args => 
        {
          val result = validateArguments(parameterWithTypes, args, context).right.flatMap(arguments => 
            eval(literalExpression, context.copy(variables = context.variables ++ arguments)) 
          )
            
          result match {
            case Right(value) => DefaultValueMapper.instance.toVal(value)
            case Left(error)  => ValError(error.toString)
          }
        } 
    )
  }
  
  private def validateArguments(parameters: Map[String, String], args: List[Val], context: EvalContext): Either[Failure, List[(String, Val)]] = 
  {
    mapEither[((String, String), Val), (String, Val)](parameters.zip(args), { case ((name, typeRef), arg) => 
      TypeChecker.isOfType(arg, typeRef, context)
        .right
        .map(name -> _)
    })
  }
  
}