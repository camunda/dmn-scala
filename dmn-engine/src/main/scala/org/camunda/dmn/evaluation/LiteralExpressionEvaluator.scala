package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.feel._
import org.camunda.feel.interpreter.{
  RootContext,
  ValFunction,
  DefaultContext,
  Val
}
import org.camunda.bpm.model.dmn.instance.{LiteralExpression, UnaryTests}
import scala.Left
import scala.Right
import org.camunda.dmn.parser.ParsedLiteralExpression

class LiteralExpressionEvaluator(feelEngine: FeelEngine) {

  def evalExpression(literalExpression: ParsedLiteralExpression,
                     context: EvalContext): Either[Failure, Val] = {
    evalExpression(literalExpression.expression, context)
  }

  def evalExpression(expression: ParsedExpression,
                     context: EvalContext): Either[Failure, Val] = {
    val functions = context.variables
      .filter { case (k, v) => v.isInstanceOf[ValFunction] }
      .map { case (k, f) => k -> List(f.asInstanceOf[ValFunction]) }

    val evalContext =
      DefaultContext(variables = context.variables, functions = functions)

    feelEngine.eval(expression, evalContext) match {
      case EvalValue(value: Val) => Right(value)
      case EvalValue(value) =>
        Left(Failure(s"expected value but found '$value'"))
      case EvalFailure(msg)  => Left(Failure(msg))
      case ParseFailure(msg) => Left(Failure(msg))
    }
  }

}
