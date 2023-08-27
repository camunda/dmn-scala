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
import org.camunda.dmn.parser.{
  ParsedBusinessKnowledgeModel,
  ParsedBusinessKnowledgeModelReference,
  ParsedDecision,
  ParsedDecisionReference,
  ParsedDecisionLogic,
  ParsedDecisionLogicContainerReference}
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

            val decisionEvaluationContext = context.copy(
              variables = context.variables
                ++ decisionResults ++ functions,
                //                ++ embeddedFunctions ++ importedFunctions,
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
    mapEither(requiredDecisions,
      (decisionRef: ParsedDecisionReference) => evalDecision(decisionRef.resolve(), context)
      .map(maybeWrapResult(decisionRef, _)))
  }

  private def evalRequiredKnowledge(
    requiredBkms: Iterable[ParsedBusinessKnowledgeModelReference],
    context: EvalContext): Either[Failure, List[(String, Val)]] = {
    mapEither(requiredBkms,
      (bkm: ParsedBusinessKnowledgeModelReference) => evalBkm(bkm.resolve(), context)
        .map(maybeWrapResult(bkm, _)))
  }

  private def maybeWrapResult(
    reference: ParsedDecisionLogicContainerReference[_], result: (String, Val)) =
    reference.importedModelName match {
      case Some(importName) =>
        val ctx = result._2 match {
          case func: ValFunction => StaticContext(
            variables = Map.empty,
            functions = Map(result._1 -> List(func))
          )
          case _ => StaticContext(
            variables = Map(result._1 -> result._2),
            functions = Map.empty
          )
        }
        importName -> ValContext(ctx)
      case _ => result
    }
}
