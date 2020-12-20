package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.feel._
import org.camunda.feel.syntaxtree.{
  ParsedExpression,
  Val,
  ValError,
  ValFunction
}
import org.camunda.bpm.model.dmn.instance.{LiteralExpression, UnaryTests}

import scala.Left
import scala.Right
import org.camunda.dmn.parser.ParsedLiteralExpression
import org.camunda.dmn.Audit.SingleEvaluationResult
import org.camunda.feel.context.Context.StaticContext

class LiteralExpressionEvaluator(feelEngine: FeelEngine) {

  def evalExpression(literalExpression: ParsedLiteralExpression,
                     context: EvalContext): Either[Failure, Val] = {
    val result = evalExpression(literalExpression.expression, context)
    context.audit(literalExpression, result)
    result
  }

  def evalExpression(expression: ParsedExpression,
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
