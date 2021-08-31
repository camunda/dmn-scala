package org.camunda.dmn.camunda.plugin

import java.io.InputStream
import java.util.stream.Collectors
import java.util.{List => JList, Map => JMap}
import org.camunda.bpm.dmn.engine.impl.{
  DefaultDmnEngineConfiguration,
  DmnDecisionResultEntriesImpl,
  DmnDecisionResultImpl
}
import org.camunda.bpm.dmn.engine._
import org.camunda.bpm.engine.ProcessEngineException
import org.camunda.bpm.engine.impl.dmn.entity.repository.{
  DecisionDefinitionEntity,
  DecisionRequirementsDefinitionEntity
}
import org.camunda.bpm.engine.impl.util.ParseUtil
import org.camunda.bpm.engine.variable.Variables
import org.camunda.bpm.engine.variable.context.VariableContext
import org.camunda.bpm.model.dmn.DmnModelInstance
import org.camunda.bpm.model.dmn.instance.Decision
import org.camunda.dmn.Audit.AuditLog
import org.camunda.dmn.DmnEngine
import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.{
  ParsedDecisionTable,
  ParsedDmn,
  ParsedList,
  ParsedRelation
}

import scala.collection.JavaConverters._
import scala.util.Either

object CamundaDmnEngine {

  type EvalListener = (DmnDecision, AuditLog) => Unit

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

        dmn.decisionsById(id).logic

        val result: DecisionResult = engine.eval(dmn, id, variables)

        result match {
          case Left(EvalFailure(failure, _)) =>
            throw new ProcessEngineException(failure.message)
          case Right(result) =>
            onEval(decision, result.auditLog)
            createDecisionResult(result, decision)
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
      case NilResult(_) => null
      case Result(value, _) =>
        value match {
          case list: List[_] if (hasCollectResult(evaluatedDecision)) => {
            val entries = list.map(item =>
              createDecisionResultEntries(item, evaluatedDecision))
            new DmnDecisionResultImpl(entries.asJava)
          }
          case list: JList[_] if (hasCollectResult(evaluatedDecision)) => {
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

  private def hasCollectResult(decision: DmnDecision): Boolean =
    decision.getDecisionLogic match {
      case DmnScalaDecisionLogic(dmn) =>
        dmn.decisionsById(decision.getKey).logic match {
          case l: ParsedDecisionTable => true
          case l: ParsedRelation      => true
          case l: ParsedList          => true
          case _                      => false
        }
      case _ => false
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
