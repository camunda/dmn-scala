package org.camunda.dmn.camunda.plugin

import org.camunda.bpm.dmn.engine.DmnDecision
import org.camunda.bpm.dmn.engine.delegate._
import org.camunda.bpm.dmn.engine.impl.delegate._
import org.camunda.bpm.dmn.engine.impl.{
  DmnDecisionTableInputImpl,
  DmnDecisionTableOutputImpl,
  DmnDecisionTableRuleImpl
}
import org.camunda.bpm.engine.ProcessEngineException
import org.camunda.bpm.engine.variable.Variables
import org.camunda.bpm.engine.variable.value.TypedValue
import org.camunda.dmn.Audit._
import org.camunda.dmn.parser.ParsedDecisionTable
import org.camunda.feel.impl.{DefaultValueMapper, JavaValueMapper}
import org.camunda.feel.syntaxtree.Val
import org.camunda.feel.valuemapper.ValueMapper._

import scala.jdk.CollectionConverters._

class CamundaDmnHistoryListener(listener: () => DmnDecisionEvaluationListener) {

  // The Camunda history event producer expect decisions of type DmnDecision.
  // Since this is not part of the DMN engine, I need to pass it from the evaluation.

  def onEvalDecision(decision: DmnDecision, auditLog: AuditLog): Unit = {
    transformLogToHistoryEvents(decision, auditLog)
  }

  private def decisionById(decision: DmnDecision,
                           id: String): Option[DmnDecision] = {
    Option(decision)
      .map(getDecisionsById(_).get(id))
      .getOrElse(throw new ProcessEngineException(
        "no decision is set in history listener"))
  }

  private def getDecisionsById(
      decision: DmnDecision): Map[String, DmnDecision] = {

    decision.getRequiredDecisions.asScala
      .flatMap(getDecisionsById)
      .toMap + (decision.getKey -> decision)
  }

  // Transforming the audit log from the DMN engine to Camunda history events.

  private def transformLogToHistoryEvents(decision: DmnDecision,
                                          log: AuditLog): Unit = {
    val evalEvent = new DmnDecisionEvaluationEventImpl();

    val rootDecision = decisionById(decision, log.rootEntry.id).getOrElse {
      throw new ProcessEngineException(
        s"no decision found with id '${log.rootEntry.id}'")
    }

    val decisionResult = createEvaluationEvent(rootDecision, log.rootEntry)
    evalEvent.setDecisionResult(decisionResult)

    val requiredResults = log.requiredEntries
      .flatMap(e => decisionById(decision, e.id).map(d => d -> e))
      .map { case (decision, entry) => createEvaluationEvent(decision, entry) }

    evalEvent.setRequiredDecisionResults(requiredResults.asJava)
    evalEvent.setExecutedDecisionElements(
      evalEvent.getDecisionResult.getExecutedDecisionElements + requiredResults
        .map(_.getExecutedDecisionElements)
        .sum)

    listener().notify(evalEvent)
  }

  private def createEvaluationEvent(
      decision: DmnDecision,
      entry: AuditLogEntry): DmnDecisionLogicEvaluationEvent = {

    entry.result match {
      case decisionTableResult: DecisionTableEvaluationResult =>
        createDecisionTableEvaluationEvent(decision, entry, decisionTableResult)
      case other => createMiscEvaluationEvent(decision, entry, other)
    }
  }

  private def createDecisionTableEvaluationEvent(
      decision: DmnDecision,
      entry: AuditLogEntry,
      evalDecisionTable: DecisionTableEvaluationResult)
    : DmnDecisionLogicEvaluationEvent = {

    val evalEvent = new DmnDecisionTableEvaluationEventImpl()
    evalEvent.setDecisionTable(decision)

    evalEvent.setInputs(
      evalDecisionTable.inputs.map(createDecisionTableInputEvent).toList.asJava)

    evalEvent.setMatchingRules(
      evalDecisionTable.matchedRules
        .map(createDecitionTableRuleEvent)
        .toList
        .asJava)

    val decisionTable = entry.decisionLogic.asInstanceOf[ParsedDecisionTable]
    Option(decisionTable.aggregation).map { _ =>
      evalEvent.setCollectResultValue(toTypedValue(evalDecisionTable.result))
      evalEvent.setCollectResultName(entry.id)
    }

    evalEvent.setExecutedDecisionElements(
      decisionTable.rules.size * decisionTable.outputs.size)

    evalEvent
  }

  private def createDecisionTableInputEvent(
      evalInput: EvaluatedInput): DmnEvaluatedInput = {

    val input = new DmnDecisionTableInputImpl()
    input.setId(evalInput.input.id)
    input.setName(evalInput.input.name)

    val evalEvent = new DmnEvaluatedInputImpl(input)
    evalEvent.setValue(toTypedValue(evalInput.value))

    evalEvent
  }

  private def createDecitionTableRuleEvent(
      evalRule: EvaluatedRule): DmnEvaluatedDecisionRule = {
    val rule = new DmnDecisionTableRuleImpl()
    rule.setId(evalRule.rule.id)

    val evalEvent = new DmnEvaluatedDecisionRuleImpl(rule)
    evalEvent.setOutputEntries(
      evalRule.outputs.map(createDecisionTableOutputEvent).toMap.asJava)

    evalEvent
  }

  private def createDecisionTableOutputEvent(
      evalOutput: EvaluatedOutput): (String, DmnEvaluatedOutput) = {

    val dmnOutput = new DmnDecisionTableOutputImpl()
    dmnOutput.setId(evalOutput.output.id)
    dmnOutput.setName(evalOutput.output.label)
    dmnOutput.setOutputName(evalOutput.output.name)

    val evalEvent =
      new DmnEvaluatedOutputImpl(dmnOutput, toTypedValue(evalOutput.value))

    evalOutput.output.name -> evalEvent
  }

  private def createMiscEvaluationEvent(
      decision: DmnDecision,
      entry: AuditLogEntry,
      evalResult: EvaluationResult): DmnDecisionLogicEvaluationEvent = {

    val evalEvent = new DmnDecisionLiteralExpressionEvaluationEventImpl()

    evalEvent.setDecision(decision)
    evalEvent.setOutputName(entry.id)
    evalEvent.setOutputValue(toTypedValue(evalResult.result))
    evalEvent.setExecutedDecisionElements(1)

    evalEvent
  }

  val valueMapper = CompositeValueMapper(
    List(DefaultValueMapper.instance, new JavaValueMapper())
  )

  private def toTypedValue(value: Val): TypedValue =
    valueMapper.unpackVal(value) match {
      case value: Long    => Variables.longValue(value)
      case value: Double  => Variables.doubleValue(value)
      case value: String  => Variables.stringValue(value)
      case value: Boolean => Variables.booleanValue(value)
      case value          => Variables.untypedValue(value)
    }

}
