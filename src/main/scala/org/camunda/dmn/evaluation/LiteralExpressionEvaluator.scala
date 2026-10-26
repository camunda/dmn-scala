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
import org.camunda.feel.context.{Context, FunctionProvider, VariableProvider}
import org.camunda.feel.syntaxtree.{Val, ValBoolean, ValFunction}

class LiteralExpressionEvaluator(feelEngine: FeelEngineApi) {

  import LiteralExpressionEvaluator.VariablesContext

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
    feelEngine.evaluate(
      expression = expression,
      context = new VariablesContext(context.variables)
    ) match {
      case SuccessfulEvaluationResult(result: Val, _) => Right(result)
      case SuccessfulEvaluationResult(other, _) => Left(Failure(s"expected value but found '$other'"))
      case FailedEvaluationResult(failure, _) => Left(Failure(failure.message))
    }
  }

}

object LiteralExpressionEvaluator {

  // Exposes the variables to the FEEL engine without copying them. The engine
  // copies the variables of a StaticContext into a new map on every
  // evaluation, which dominates the cost of evaluating large decision tables.
  // Variables that hold a function are also provided as functions.
  private class VariablesContext(variables: Map[String, Any])
      extends Context
      with VariableProvider
      with FunctionProvider {

    override def variableProvider: VariableProvider = this

    override def functionProvider: FunctionProvider = this

    override def getVariable(name: String): Option[Any] = variables.get(name)

    override def keys: Iterable[String] = variables.keys

    override def getFunctions(name: String): List[ValFunction] =
      variables.get(name) match {
        case Some(function: ValFunction) => List(function)
        case _                           => List.empty
      }

    override def functionNames: Iterable[String] =
      variables.collect { case (name, _: ValFunction) => name }
  }

}
