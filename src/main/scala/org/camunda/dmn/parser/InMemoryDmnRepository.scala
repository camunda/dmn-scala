package org.camunda.dmn.parser

import org.camunda.dmn.DmnEngine.Failure

import scala.collection.mutable

class InMemoryDmnRepository extends DmnRepository {

  private val parsedDmnByNamespace = mutable.Map.empty[String, ParsedDmn]

  override def getBusinessKnowledgeModel(namespace: String, bkmId: String): Either[Failure, ParsedBusinessKnowledgeModel] = {
    parsedDmnByNamespace.get(namespace) match {
      case None => Left(Failure(s"No BKM found with namespace '$namespace'."))
      case Some(parsedDmn) =>
        parsedDmn.bkms.find(_.id == bkmId) match {
          case None => Left(Failure(s"No BKM found with id '$bkmId' in namespace '$namespace'."))
          case Some(bkm) => Right(bkm)
        }
    }
  }

  override def getDecision(namespace: String, decisionId: String): Either[Failure, ParsedDecision] = {
    parsedDmnByNamespace.get(namespace) match {
      case None => Left(Failure(s"No decision found with namespace '$namespace'."))
      case Some(parsedDmn) =>
        parsedDmn.decisions.find(_.id == decisionId) match {
          case None => Left(Failure(s"No decision found with id '$decisionId' in namespace '$namespace'."))
          case Some(decision) => Right(decision)
        }
    }
  }

  override def put(dmn: ParsedDmn): Unit = {
    parsedDmnByNamespace.put(dmn.namespace, dmn)
  }
}
