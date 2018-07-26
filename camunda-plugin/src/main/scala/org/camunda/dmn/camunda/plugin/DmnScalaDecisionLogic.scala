package org.camunda.dmn.camunda.plugin

import org.camunda.bpm.dmn.engine.DmnDecisionLogic
import org.camunda.dmn.parser.ParsedDmn

case class DmnScalaDecisionLogic(val dmn: ParsedDmn) extends DmnDecisionLogic {

  // parsed DMN is required for evaluation

}
