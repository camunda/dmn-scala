package org.camunda.dmn.camunda.plugin

import scala.collection.JavaConverters._
import scala.beans.BeanProperty

import org.camunda.bpm.engine.impl.cfg.AbstractProcessEnginePlugin
import org.camunda.bpm.engine.impl.cfg.ProcessEngineConfigurationImpl
import org.camunda.dmn.DmnEngine;
import org.camunda.bpm.dmn.engine.DmnEngineConfiguration
import org.camunda.bpm.dmn.engine.impl.DefaultDmnEngineConfiguration
import org.camunda.bpm.engine.impl.history.parser.HistoryDecisionEvaluationListener
import org.camunda.bpm.engine.ProcessEngineException

class CamundaDmnEnginePlugin extends AbstractProcessEnginePlugin {

  var historyListener: HistoryDecisionEvaluationListener = _

  override def preInit(config: ProcessEngineConfigurationImpl) {

    val auditLogListener = new CamundaDmnHistoryListener(() => {
      Option(historyListener)
        .getOrElse(
          throw new ProcessEngineException("history listener is not created"))
    })

    val dmnEngine = new CamundaDmnEngine(
      new DmnEngine(auditLogListeners = List(auditLogListener)),
      auditLogListener.onEvalDecision)

    // replace the default Camunda DMN engine
    config.setDmnEngine(dmnEngine)
  }

  override def postInit(config: ProcessEngineConfigurationImpl) {
    // history level and event producer are set on init()
    historyListener = new HistoryDecisionEvaluationListener(
      config.getDmnHistoryEventProducer,
      config.getHistoryLevel)
  }

}
