package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.feel._
import org.camunda.feel.interpreter.{ RootContext, ValFunction }
import org.camunda.bpm.model.dmn.instance.{ LiteralExpression, UnaryTests }
import scala.Left
import scala.Right
import org.camunda.feel.interpreter.DefaultContext
import org.camunda.dmn.parser.ParsedLiteralExpression

class LiteralExpressionEvaluator(feelEngine: FeelEngine) {

  def evalExpression(literalExpression: ParsedLiteralExpression, context: EvalContext): Either[Failure, Any] = {
    evalExpression(literalExpression.expression, context)
  }

  def evalExpression(expression: ParsedExpression, context: EvalContext): Either[Failure, Any] = {
    val functions = context.variables
      .filter { case (k, v) => v.isInstanceOf[ValFunction] }
      .map { case (k, f) => k -> List(f.asInstanceOf[ValFunction]) }

    val evalContext = DefaultContext(
      variables = context.variables,
      functions = functions)

    feelEngine.eval(expression, evalContext) match {
      case EvalValue(value)  => Right(value)
      case EvalFailure(msg)  => Left(Failure(msg))
      case ParseFailure(msg) => Left(Failure(msg))
    }
  }

}
