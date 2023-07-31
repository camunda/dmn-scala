package org.camunda.dmn.parser

import org.camunda.dmn.DmnEngine.Failure

trait DmnRepository {

  def getBusinessKnowledgeModel(namespace: String, bkmId: String): Either[Failure, ParsedBusinessKnowledgeModel]

  def getDecision(namespace: String, decisionId: String): Either[Failure, ParsedDecision]

  def put(dmn: ParsedDmn): Unit

}
