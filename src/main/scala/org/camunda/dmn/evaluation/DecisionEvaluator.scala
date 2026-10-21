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
import org.camunda.feel.syntaxtree.{Val, ValFunction}
import org.camunda.dmn.parser.{
  ParsedDecision,
  ParsedDecisionLogic,
  ParsedBusinessKnowledgeModel
}

import scala.collection.mutable
import scala.util.control.TailCalls._

class DecisionEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val],
    evalBkm: (ParsedBusinessKnowledgeModel,
              EvalContext) => Either[Failure, (String, ValFunction)]) {

  def eval(decision: ParsedDecision,
           context: EvalContext): Either[Failure, Val] = {

    // The results of the decisions evaluated so far, by decision id. A
    // required decision gets the same variables on every path, so it is
    // evaluated once and its result is reused (camunda/dmn-scala#394). Kept
    // per evaluation, not across evaluations.
    val evaluatedDecisions = mutable.Map.empty[String, (String, Val)]

    evalDecision(decision, context, evaluatedDecisions).result
      .map { case (name, result) => result }
  }

  // Trampolined via TailCalls: evalDecision <-> evalRequiredDecisions is
  // mutually recursive with depth equal to the length of the decision
  // requirements chain, which can be in the thousands. Ordinary recursion
  // here overflows the JVM stack (camunda/dmn-scala#335).
  private def evalDecision(
      decision: ParsedDecision,
      context: EvalContext,
      evaluatedDecisions: mutable.Map[String, (String, Val)])
    : TailRec[Either[Failure, (String, Val)]] = {

    evaluatedDecisions.get(decision.id) match {
      case Some(result) => done(Right(result))
      case None =>
        tailcall(
          evalRequiredDecisions(decision.requiredDecisions,
                                context,
                                evaluatedDecisions))
          .map(_.flatMap(decisionResults => {
            evalRequiredKnowledge(decision.requiredBkms, context)
              .flatMap(functions => {

                val decisionEvaluationContext = context.copy(
                  variables = context.variables ++ decisionResults ++ functions,
                  currentElement = decision)

                eval(decision.logic, decisionEvaluationContext)
                  .flatMap(
                    result =>
                      decision.resultType
                        .map(typeRef => TypeChecker.isOfType(result, typeRef))
                        .getOrElse(Right(result)))
                  .map(decision.resultName -> _)
              })
          }))
          .map(_.map { result =>
            evaluatedDecisions.put(decision.id, result)
            result
          })
    }
  }

  private def evalRequiredDecisions(
      requiredDecisions: Iterable[ParsedDecision],
      context: EvalContext,
      evaluatedDecisions: mutable.Map[String, (String, Val)])
    : TailRec[Either[Failure, List[(String, Val)]]] = {
    mapEitherTailRec(
      requiredDecisions,
      (d: ParsedDecision) => evalDecision(d, context, evaluatedDecisions))
  }

  private def evalRequiredKnowledge(
      requiredBkms: Iterable[ParsedBusinessKnowledgeModel],
      context: EvalContext): Either[Failure, List[(String, ValFunction)]] = {
    mapEither(requiredBkms,
              (bkm: ParsedBusinessKnowledgeModel) => evalBkm(bkm, context))
  }

}
