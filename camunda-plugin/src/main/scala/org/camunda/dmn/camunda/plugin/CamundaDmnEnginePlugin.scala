package org.camunda.dmn.camunda.plugin

import scala.collection.JavaConverters._
import scala.beans.BeanProperty

import org.camunda.bpm.engine.impl.cfg.AbstractProcessEnginePlugin
import org.camunda.bpm.engine.impl.cfg.ProcessEngineConfigurationImpl
import org.camunda.dmn.DmnEngine;
import org.camunda.bpm.dmn.engine.DmnEngineConfiguration
import org.camunda.bpm.dmn.engine.impl.DefaultDmnEngineConfiguration

class CamundaDmnEnginePlugin extends AbstractProcessEnginePlugin {
  
  override def preInit(config: ProcessEngineConfigurationImpl) {
    
    val dmnEngine = new CamundaDmnEngine(new DmnEngine)
    
    // replace the default Camunda DMN engine
    config.setDmnEngine(dmnEngine)
  }
  
}