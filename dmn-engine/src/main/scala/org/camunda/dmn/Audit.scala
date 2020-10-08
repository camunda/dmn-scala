package org.camunda.dmn

import org.camunda.dmn.parser._
import org.camunda.feel.syntaxtree.Val

object Audit {

  trait AuditLogListener {

    def onEval(log: AuditLog)
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
