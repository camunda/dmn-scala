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
package org.camunda.dmn

import org.camunda.dmn.parser._
import org.camunda.feel.syntaxtree.Val

object Audit {

  trait AuditLogListener {

    def onEval(log: AuditLog)

    def onFailure(log: AuditLog)
  }

  case class AuditLog(dmn: ParsedDmn, entries: List[AuditLogEntry]) {

    val rootEntry = entries.last
    val requiredEntries = entries.dropRight(1)
  }

  case class AuditLogEntry(id: String,
                           name: String,
                           decisionLogic: ParsedDecisionLogic,
                           result: EvaluationResult)

  ///// evaluation results

  sealed trait EvaluationResult {
    val result: Val
  }

  case class SingleEvaluationResult(override val result: Val)
      extends EvaluationResult

  case class ContextEvaluationResult(entries: Map[String, Val],
                                     override val result: Val)
      extends EvaluationResult

  ///// decision table results

  case class DecisionTableEvaluationResult(inputs: List[EvaluatedInput],
                                           matchedRules: List[EvaluatedRule],
                                           override val result: Val)
      extends EvaluationResult

  case class EvaluatedInput(input: ParsedInput, value: Val)

  case class EvaluatedOutput(output: ParsedOutput, value: Val)

  case class EvaluatedRule(rule: ParsedRule, outputs: List[EvaluatedOutput])

}
