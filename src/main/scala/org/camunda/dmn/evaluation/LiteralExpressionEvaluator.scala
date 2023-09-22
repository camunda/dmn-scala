/*
 * Copyright © 2022 Camunda Services GmbH (info@camunda.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser._
import org.camunda.feel
import org.camunda.feel.api.{FailedEvaluationResult, FeelEngineApi, SuccessfulEvaluationResult}
import org.camunda.feel.context.Context.StaticContext
import org.camunda.feel.syntaxtree.{Val, ValBoolean, ValFunction}

class LiteralExpressionEvaluator(feelEngine: FeelEngineApi) {

  def evalExpression(literalExpression: ParsedLiteralExpression,
                     context: EvalContext): Either[Failure, Val] = {

    val result = evalExpression(literalExpression.expression, context)
    context.audit(literalExpression, result)
    result
  }

  def evalExpression(expression: ParsedExpression,
                     context: EvalContext): Either[Failure, Val] = {
    expression match {
      case FeelExpression(exp) => evalFeelExpression(exp, context)
      case EmptyExpression => Right(ValBoolean(true))
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

    feelEngine.evaluate(
      expression = expression,
      context = evalContext
    ) match {
      case SuccessfulEvaluationResult(result: Val, _) => Right(result)
      case SuccessfulEvaluationResult(other, _) => Left(Failure(s"expected value but found '$other'"))
      case FailedEvaluationResult(failure, _) => Left(Failure(failure.message))
    }
  }

}
