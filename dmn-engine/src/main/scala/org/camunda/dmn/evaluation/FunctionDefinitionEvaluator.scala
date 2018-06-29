package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{ FunctionDefinition, FormalParameter, Expression, LiteralExpression }
import org.camunda.feel.interpreter.{ ValFunction, ValError, DefaultValueMapper, Val }
import org.camunda.dmn.parser.ParsedLiteralExpression
import org.camunda.dmn.parser.ParsedFunctionDefinition
import org.camunda.feel.ParsedExpression

class FunctionDefinitionEvaluator(eval: (ParsedExpression, EvalContext) => Either[Failure, Any]) {

  def eval(function: ParsedFunctionDefinition, context: EvalContext): Either[Failure, Any] = {
    Right(createFunction(function.expression, function.parameters, context))
  }

  private def createFunction(expression: ParsedExpression, parameters: Iterable[(String, String)], context: EvalContext): ValFunction = {
    val parameterNames = parameters.map(_._1).toList

    ValFunction(
      params = parameterNames,
      invoke = args => {
        val result = validateArguments(parameters, args, context).right.flatMap(arguments =>
          eval(expression, context.copy(variables = context.variables ++ arguments)))

        result match {
          case Right(value) => DefaultValueMapper.instance.toVal(value)
          case Left(error)  => ValError(error.toString)
        }
      })
  }

  private def validateArguments(parameters: Iterable[(String, String)], args: List[Val], context: EvalContext): Either[Failure, List[(String, Val)]] = {
    mapEither[((String, String), Val), (String, Val)](parameters.zip(args), {
      case ((name, typeRef), arg) =>
        TypeChecker.isOfType(arg, typeRef, context)
          .right
          .map(name -> _)
    })
  }

}
