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
import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser.{ParsedExpression, ParsedFunctionDefinition}
import org.camunda.feel.syntaxtree.{Val, ValError, ValFunction}

class FunctionDefinitionEvaluator(
    eval: (ParsedExpression, EvalContext) => Either[Failure, Val]) {

  def eval(function: ParsedFunctionDefinition,
           context: EvalContext): Either[Failure, Val] = {
    Right(createFunction(function.expression, function.parameters, context))
  }

  private def createFunction(expression: ParsedExpression,
                             parameters: Iterable[(String, String)],
                             context: EvalContext): ValFunction = {
    val parameterNames = parameters.map(_._1).toList

    ValFunction(
      params = parameterNames,
      invoke = args => {
        val result = validateArguments(parameters, args, context).flatMap(
          arguments =>
            eval(expression,
                 context.copy(variables = context.variables ++ arguments)))

        result match {
          case Right(value)  => value
          case Left(failure) => ValError(failure.message)
        }
      }
    )
  }

  private def validateArguments(
      parameters: Iterable[(String, String)],
      args: List[Val],
      context: EvalContext): Either[Failure, List[(String, Val)]] = {
    mapEither[((String, String), Val), (String, Val)](parameters.zip(args), {
      case ((name, typeRef), arg) =>
        TypeChecker
          .isOfType(arg, typeRef)
          .map(name -> _)
    })
  }

}
