package org.camunda.dmn.parser

import org.camunda.dmn.DmnEngine.Failure

object StatelessDmnRepository extends DmnRepository {
  override def getBusinessKnowledgeModel(namespace: String, bkmId: String): Either[Failure, ParsedBusinessKnowledgeModel] =
    Left(Failure("No models are stored. This is a stateless repository."))


  override def getDecision(namespace: String, decisionId: String): Either[Failure, ParsedDecision] =
    Left(Failure("No models are stored. This is a stateless repository."))

  override def put(dmn: ParsedDmn): Unit = {
    // no-op
  }
}
