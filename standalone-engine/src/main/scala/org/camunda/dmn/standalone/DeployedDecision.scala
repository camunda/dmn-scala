package org.camunda.dmn.standalone

import org.camunda.bpm.model.dmn.instance.Decision
import org.camunda.dmn.parser.ParsedDmn

case class DeployedDecision(parsedDmn: ParsedDmn, decisionId: String, decisionName: String, resource: String) {
  
}