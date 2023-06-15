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
import org.camunda.feel.syntaxtree.{Val, ValContext, ValFunction}
import org.camunda.dmn.parser.{ParsedBusinessKnowledgeModel, ParsedDecision, ParsedDecisionLogic}
import org.camunda.feel.context.Context.StaticContext

class DecisionEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val],
    evalBkm: (ParsedBusinessKnowledgeModel,
              EvalContext) => Either[Failure, (String, ValFunction)]) {

  def eval(decision: ParsedDecision,
           context: EvalContext): Either[Failure, Val] = {

    evalDecision(decision, context)
      .map { case (name, result) => result }
  }

  private def evalDecision(
      decision: ParsedDecision,
      context: EvalContext): Either[Failure, (String, Val)] = {

    evalRequiredDecisions(decision.requiredDecisions, context)
      .flatMap(decisionResults => {
        evalRequiredKnowledge(decision.requiredBkms, context)
          .flatMap(functions => {

            // todo: replace the hack to wrap the imported BKMs into a context, maybe move to the BKM evaluation logic
            val importedFunctions = functions
              .filter { case (name, _) => name.contains(".") }
              .map { case (name, function) =>
                val Array(prefix: String, functionName: String) = name.split('.')
                prefix -> ValContext(StaticContext(
                  variables = Map.empty,
                  functions = Map(functionName -> List(function))
                ))
              }

            val embeddedFunctions = functions.filterNot { case (name, _) => name.contains(".") }

            val decisionEvaluationContext = context.copy(
              variables = context.variables ++ decisionResults ++ embeddedFunctions ++ importedFunctions,
              currentElement = decision)

            eval(decision.logic, decisionEvaluationContext)
              .flatMap(
                result =>
                  decision.resultType
                    .map(typeRef => TypeChecker.isOfType(result, typeRef))
                    .getOrElse(Right(result)))
              .map(decision.resultName -> _)
          })
      })
  }

  private def evalRequiredDecisions(
      requiredDecisions: Iterable[ParsedDecision],
      context: EvalContext): Either[Failure, List[(String, Val)]] = {
    mapEither(requiredDecisions,
              (d: ParsedDecision) => evalDecision(d, context))
  }

  private def evalRequiredKnowledge(
      requiredBkms: Iterable[ParsedBusinessKnowledgeModel],
      context: EvalContext): Either[Failure, List[(String, ValFunction)]] = {
    mapEither(requiredBkms,
              (bkm: ParsedBusinessKnowledgeModel) => evalBkm(bkm, context))
  }

}
