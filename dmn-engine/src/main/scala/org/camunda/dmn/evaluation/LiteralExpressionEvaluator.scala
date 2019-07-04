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
import org.camunda.dmn.Audit.SingleEvaluationResult

class LiteralExpressionEvaluator(feelEngine: FeelEngine) {

  def evalExpression(literalExpression: ParsedLiteralExpression,
                     context: EvalContext): Either[Failure, Val] = {

    evalExpression(literalExpression.expression, context).right.map { result =>
      context.audit(literalExpression, SingleEvaluationResult(result))

      result
    }
  }

  def evalExpression(expression: ParsedExpression,
                     context: EvalContext): Either[Failure, Val] = {
    val functions = context.variables
      .filter { case (k, v) => v.isInstanceOf[ValFunction] }
      .map { case (k, f) => k -> List(f.asInstanceOf[ValFunction]) }

    val evalContext =
      DefaultContext(variables = context.variables, functions = functions)

    feelEngine.eval(expression, evalContext) match {
      case Right(v: Val)                     => Right(v)
      case Right(other)                      => Left(Failure(s"expected value but found '$other'"))
      case Left(FeelEngine.Failure(message)) => Left(Failure(message))
    }
  }

}
