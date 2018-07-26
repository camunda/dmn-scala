package org.camunda.dmn.standalone

import org.camunda.dmn._
import org.camunda.dmn.DmnEngine
import org.camunda.dmn.DmnEngine.Failure
import java.io.InputStream

trait DecisionRepository {

  val dmnEngine: DmnEngine

  def parseDecision(
      stream: InputStream,
      resource: String): Either[Failure, List[(String, DeployedDecision)]] = {
    dmnEngine.parse(stream) match {
      case Left(f) => Left(Failure(s"Fail to parse file '$resource': $f"))
      case Right(dmn) =>
        Right(
          dmn.decisions
            .map(d => d.id -> DeployedDecision(dmn, d.id, d.name, resource))
            .toList)
    }
  }

  def init: Unit

  def getDecisions: List[DeployedDecision]

  def getDecisionById(id: String): Option[DeployedDecision]

  def getDecisionByName(name: String): Option[DeployedDecision]

  def insertDecisions(stream: InputStream,
                      resource: String): Either[Failure, List[DeployedDecision]]

  def removeResource(resource: String): Either[Failure, List[DeployedDecision]]

}
