package org.camunda.dmn.camunda.plugin

import scala.collection.JavaConverters._

import java.io.InputStream
import java.util.{List => JList, Map => JMap}

import org.camunda.dmn.DmnEngine
import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.ParsedDmn
import org.camunda.bpm.model.dmn.DmnModelInstance
import org.camunda.bpm.model.dmn.instance.Decision
import org.camunda.bpm.dmn.engine.{
  DmnEngineConfiguration,
  DmnDecisionResult,
  DmnDecision,
  DmnDecisionResultEntries,
  DmnDecisionRequirementsGraph,
  DmnDecisionTableResult
}
import org.camunda.bpm.dmn.engine.impl.{
  DefaultDmnEngineConfiguration,
  DmnDecisionResultImpl,
  DmnDecisionResultEntriesImpl
}
import org.camunda.bpm.engine.impl.dmn.entity.repository.{
  DecisionRequirementsDefinitionEntity,
  DecisionDefinitionEntity
}
import org.camunda.bpm.engine.ProcessEngineException
import org.camunda.bpm.engine.variable.Variables
import org.camunda.bpm.engine.variable.context.VariableContext
import org.camunda.bpm.engine.impl.util.ParseUtil
import java.util.stream.Collectors
import scala.util.Either

object CamundaDmnEngine {

  type EvalListener = (
      DmnDecision,
      String,
      () => Either[Failure, EvalResult]) => Either[Failure, EvalResult]

}

class CamundaDmnEngine(engine: DmnEngine, onEval: CamundaDmnEngine.EvalListener)
    extends org.camunda.bpm.dmn.engine.DmnEngine {

  override def parseDecisions(inputStream: InputStream): JList[DmnDecision] = {
    parseAndCreateDecisions(inputStream).asJava
  }

  override def parseDecision(decisionKey: String,
                             inputStream: InputStream): DmnDecision = {
    parseAndCreateDecisions(inputStream)
      .find(_.getKey() == decisionKey)
      .getOrElse(throw new ProcessEngineException(
        s"no decision found with key '$decisionKey'"))
  }

  override def parseDecisionRequirementsGraph(
      inputStream: InputStream): DmnDecisionRequirementsGraph = {
    engine.parse(inputStream) match {
      case Left(failure) => throw new ProcessEngineException(failure.message)
      case Right(parsedDmn) => {

        val definitions = parsedDmn.model.getDefinitions

        val drg = new DecisionRequirementsDefinitionEntity

        drg.setKey(definitions.getId)
        drg.setName(definitions.getName)
        drg.setCategory(definitions.getNamespace)

        val decisionMap = createDecisionDefinitions(parsedDmn, Some(drg))
          .map(d => d.getKey -> d)
          .toMap
          .asJava

        drg.setDecisions(decisionMap)

        drg
      }
    }
  }

  private def parseAndCreateDecisions(
      inputStream: InputStream): List[DmnDecision] = {
    engine.parse(inputStream) match {
      case Left(failure) => throw new ProcessEngineException(failure.toString)
      case Right(parsedDmn) => {
        createDecisionDefinitions(parsedDmn).toList
      }
    }
  }

  private def createDecisionDefinitions(
      parsedDmn: ParsedDmn,
      drg: Option[DecisionRequirementsDefinitionEntity] = None)
    : Iterable[DmnDecision] = {
    val model = parsedDmn.model
    val namespace = model.getDefinitions.getNamespace

    val decisions = parsedDmn.decisions.map(decision => {
      val element: Decision = model.getModelElementById(decision.id)

      val definition = new DecisionDefinitionEntity

      definition.setKey(decision.id)
      definition.setName(decision.name)
      definition.setCategory(namespace)
      definition.setDecisionLogic(new DmnScalaDecisionLogic(parsedDmn))
      definition.setHistoryTimeToLive(
        ParseUtil.parseHistoryTimeToLive(
          element.getCamundaHistoryTimeToLiveString))
      definition.setVersionTag(element.getVersionTag)

      drg.map { drg =>
        definition.setDecisionRequirementsDefinitionId(drg.getId)
        definition.setDecisionRequirementsDefinitionKey(drg.getKey)
      }

      definition
    })

    val decisionsById = decisions.map(d => d.getKey -> d).toMap

    decisions.map { decision =>
      val parsedDecision = parsedDmn.decisionsById(decision.getKey)

      val requiredDecisions =
        parsedDecision.requiredDecisions.map(requiredDecision =>
          decisionsById(requiredDecision.id): DmnDecision)
      decision.setRequiredDecision(requiredDecisions.toList.asJava)
    }

    decisions
  }

  override def evaluateDecision(
      decision: DmnDecision,
      variables: JMap[String, Object]): DmnDecisionResult = {
    decision.getDecisionLogic match {
      case DmnScalaDecisionLogic(dmn) => {
        val id = decision.getKey
        val name = decision.getName

        val result = onEval(decision, id, () => engine.eval(dmn, id, variables))

        result match {
          case Left(failure) =>
            throw new ProcessEngineException(failure.toString)
          case Right(result) => createDecisionResult(result, decision)
        }
      }
      case other =>
        throw new ProcessEngineException(
          s"expected DmnScalaDecisionLogic but fount '$other'")
    }
  }

  override def evaluateDecision(
      decision: DmnDecision,
      variables: VariableContext): DmnDecisionResult = {
    val vars = variables
      .keySet()
      .asScala
      .map(k => k -> variables.resolve(k).getValue)
      .toMap
      .asJava

    evaluateDecision(decision, vars)
  }

  override def evaluateDecision(
      decisionKey: String,
      inputStream: InputStream,
      variables: JMap[String, Object]): DmnDecisionResult = {
    val decision = parseDecision(decisionKey, inputStream)

    evaluateDecision(decision, variables)
  }

  override def evaluateDecision(
      decisionKey: String,
      inputStream: InputStream,
      variables: VariableContext): DmnDecisionResult = {
    val decision = parseDecision(decisionKey, inputStream)

    evaluateDecision(decision, variables)
  }

  private def createDecisionResult(
      result: EvalResult,
      evaluatedDecision: DmnDecision): DmnDecisionResult = {
    result match {
      case NilResult => null
      case Result(value) =>
        value match {
          case list: List[_] => {
            val entries = list.map(item =>
              createDecisionResultEntries(item, evaluatedDecision))
            new DmnDecisionResultImpl(entries.asJava)
          }
          case list: JList[_] => {
            val entries = list
              .stream()
              .map[DmnDecisionResultEntries](item =>
                createDecisionResultEntries(item, evaluatedDecision))
              .collect(Collectors.toList())
            new DmnDecisionResultImpl(entries)
          }
          case _ => {
            val entries = createDecisionResultEntries(value, evaluatedDecision)
            new DmnDecisionResultImpl(
              List[DmnDecisionResultEntries](entries).asJava)
          }
        }
    }
  }

  private def createDecisionResultEntries(
      value: Any,
      evaluatedDecision: DmnDecision): DmnDecisionResultEntries = {
    val entry = new DmnDecisionResultEntriesImpl()

    value match {
      case m: Map[_, _] =>
        m.map {
          case (k, v) => entry.putValue(k.toString, Variables.untypedValue(v))
        }
      case m: JMap[_, _] =>
        m.forEach {
          case (k, v) => entry.putValue(k.toString, Variables.untypedValue(v))
        }
      case _ =>
        entry.putValue(evaluatedDecision.getName, Variables.untypedValue(value))
    }

    entry
  }

  override def getConfiguration: DmnEngineConfiguration = {
    val config = DmnEngineConfiguration
      .createDefaultDmnEngineConfiguration()
      .asInstanceOf[DefaultDmnEngineConfiguration]

    // by replacing the transformer, the DMN deployers use the DMN engine to parse a DMN
    config.transformer(new DmnScalaTransformer(this))
  }

  ///// not supported

  override def parseDecisions(
      dmnModelInstance: DmnModelInstance): JList[DmnDecision] =
    throw new UnsupportedOperationException

  override def parseDecision(decisionKey: String,
                             dmnModelInstance: DmnModelInstance): DmnDecision =
    throw new UnsupportedOperationException

  override def parseDecisionRequirementsGraph(
      modelInstance: DmnModelInstance): DmnDecisionRequirementsGraph =
    throw new UnsupportedOperationException

  override def evaluateDecisionTable(
      decision: DmnDecision,
      variables: JMap[String, Object]): DmnDecisionTableResult =
    throw new UnsupportedOperationException

  override def evaluateDecisionTable(
      decision: DmnDecision,
      variables: VariableContext): DmnDecisionTableResult =
    throw new UnsupportedOperationException

  override def evaluateDecisionTable(
      decisionKey: String,
      inputStream: InputStream,
      variables: JMap[String, Object]): DmnDecisionTableResult =
    throw new UnsupportedOperationException

  override def evaluateDecisionTable(
      decisionKey: String,
      inputStream: InputStream,
      variables: VariableContext): DmnDecisionTableResult =
    throw new UnsupportedOperationException

  override def evaluateDecisionTable(
      decisionKey: String,
      modelInstance: DmnModelInstance,
      variables: JMap[String, Object]): DmnDecisionTableResult =
    throw new UnsupportedOperationException

  override def evaluateDecisionTable(
      decisionKey: String,
      modelInstance: DmnModelInstance,
      variables: VariableContext): DmnDecisionTableResult =
    throw new UnsupportedOperationException

  override def evaluateDecision(
      decisionKey: String,
      modelInstance: DmnModelInstance,
      variables: JMap[String, Object]): DmnDecisionResult =
    throw new UnsupportedOperationException

  override def evaluateDecision(decisionKey: String,
                                modelInstance: DmnModelInstance,
                                variables: VariableContext): DmnDecisionResult =
    throw new UnsupportedOperationException

}
