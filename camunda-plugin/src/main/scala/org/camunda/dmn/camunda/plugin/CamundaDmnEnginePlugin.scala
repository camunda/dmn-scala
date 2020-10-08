package org.camunda.dmn.camunda.plugin

import org.camunda.bpm.engine.ProcessEngineException
import org.camunda.bpm.engine.impl.cfg.{
  AbstractProcessEnginePlugin,
  ProcessEngineConfigurationImpl
}
import org.camunda.bpm.engine.impl.history.parser.HistoryDecisionEvaluationListener
import org.camunda.dmn.DmnEngine

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
      config.getDmnHistoryEventProducer)
  }

}
