package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser._
import org.camunda.feel
import org.camunda.feel._
import org.camunda.feel.context.Context.StaticContext
import org.camunda.feel.syntaxtree.{Val, ValBoolean, ValFunction}

class LiteralExpressionEvaluator(feelEngine: FeelEngine) {

  def evalExpression(literalExpression: ParsedLiteralExpression,
                     context: EvalContext): Either[Failure, Val] = {

    val result = evalExpression(literalExpression.expression, context)
    context.audit(literalExpression, result)
    result
  }

  def evalExpression(expression: ParsedExpression,
                     context: EvalContext): Either[Failure, Val] = {
    expression match {
      case FeelExpression(exp)        => evalFeelExpression(exp, context)
      case EmptyExpression            => Right(ValBoolean(true))
      case ExpressionFailure(failure) => Left(Failure(message = failure))
      case other =>
        Left(Failure(message = s"Failed to evaluate expression '$other'"))
    }
  }

  private def evalFeelExpression(expression: feel.syntaxtree.ParsedExpression,
                                 context: EvalContext): Either[Failure, Val] = {
    val functions = context.variables
      .filter { case (k, v) => v.isInstanceOf[ValFunction] }
      .map { case (k, f) => k -> List(f.asInstanceOf[ValFunction]) }

    val evalContext =
      StaticContext(variables = context.variables, functions = functions)

    feelEngine.eval(expression, evalContext) match {
      case Right(v: Val)                     => Right(v)
      case Right(other)                      => Left(Failure(s"expected value but found '$other'"))
      case Left(FeelEngine.Failure(message)) => Left(Failure(message))
    }
  }

}
