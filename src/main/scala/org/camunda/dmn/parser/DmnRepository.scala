package org.camunda.dmn.parser

import org.camunda.dmn.DmnEngine.Failure

trait DmnRepository {

  def getBusinessKnowledgeModel(namespace: String, bkmId: String): Either[Failure, ParsedBusinessKnowledgeModel]

  def put(dmn: ParsedDmn): Unit

}
