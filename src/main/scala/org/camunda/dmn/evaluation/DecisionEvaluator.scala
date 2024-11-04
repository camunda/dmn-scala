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
import org.camunda.dmn.parser.{DmnRepository, EmbeddedBusinessKnowledgeModel, EmbeddedDecision, ImportedBusinessKnowledgeModel, ImportedDecision, ParsedBusinessKnowledgeModel, ParsedBusinessKnowledgeModelFailure, ParsedBusinessKnowledgeModelReference, ParsedDecision, ParsedDecisionFailure, ParsedDecisionLogic, ParsedDecisionReference}
import org.camunda.feel.context.Context.StaticContext

class DecisionEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val],
    evalBkm: (ParsedBusinessKnowledgeModel,
              EvalContext) => Either[Failure, (String, ValFunction)],
    repository: DmnRepository) {

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

            val decisionEvaluationContext = context.copy(
              variables = context.variables
                ++ decisionResults ++ functions,
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
    requiredDecisions: Iterable[ParsedDecisionReference],
    context: EvalContext): Either[Failure, List[(String, Val)]] = {
    mapEither[ParsedDecisionReference, (String, Val)](requiredDecisions, {
          case ImportedDecision(namespace, decisionId, importName) =>
            repository.getDecision(namespace = namespace, decisionId = decisionId)
              .flatMap(evalDecision(_, context))
              .map { case (name, result) =>
                importName -> ValContext(StaticContext(
                  variables = Map(name -> result),
                  functions = Map.empty
                ))
              }

          case ParsedDecisionFailure(_, _, failureMessage) => Left(Failure(failureMessage))
          case decision: EmbeddedDecision => evalDecision(decision, context)
        }
    )
  }

  private def evalRequiredKnowledge(
    requiredBkms: Iterable[ParsedBusinessKnowledgeModelReference],
    context: EvalContext): Either[Failure, List[(String, Val)]] = {
    mapEither[ParsedBusinessKnowledgeModelReference, (String, Val)](requiredBkms, {
          case ImportedBusinessKnowledgeModel(namespace, id, importName) =>
            repository.getBusinessKnowledgeModel(namespace = namespace, bkmId = id)
            .flatMap(evalBkm(_, context))
            .map { case (name, resultFunction) =>
              importName -> ValContext(
                StaticContext(
                  variables = Map.empty,
                  functions = Map(name -> List(resultFunction))
                )
              )
            }

          case ParsedBusinessKnowledgeModelFailure(_, _, failureMessage) => Left(Failure(failureMessage))
          case bkm: EmbeddedBusinessKnowledgeModel => evalBkm(bkm, context)
        }
    )
  }

}
