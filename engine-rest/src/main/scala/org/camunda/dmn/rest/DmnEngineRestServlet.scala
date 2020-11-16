package org.camunda.dmn.rest

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.standalone.{DeployedDecision, StandaloneEngine}
import org.json4s.{DefaultFormats, Formats}
import org.scalatra._
import org.scalatra.json._
import org.scalatra.servlet.{FileItem, FileUploadSupport, MultipartConfig}

class DmnEngineRestServlet(engine: StandaloneEngine)
    extends ScalatraServlet
    with JacksonJsonSupport
    with FileUploadSupport {

  // JSON configuration
  protected implicit lazy val jsonFormats: Formats =
    DefaultFormats.withBigDecimal
  // file upload configuration
  configureMultipartHandling(
    MultipartConfig(maxFileSize = Some(3 * 1024 * 1024)))

  // JSON data objects
  case class Decision(id: String, name: String, resource: String)

  case class DeploymentResult(deployedDecisions: Iterable[Decision],
                              failures: Iterable[Failure])

  case class DecisionEvalResult(result: Any)

  // response is JSON
  before() {
    contentType = formats("json")
  }

  // REST endpoints

  get("/decisions") {
    engine.getDecisions
      .map(decisionDto)
  }

  post("/decisions/:id/eval") {
    val id = params("id")
    val variables = parsedBody.extract[Map[String, Any]]

    engine.evalDecisionById(id, variables) match {
      case Left(failure)    => BadRequest(failure)
      case Right(NilResult) => DecisionEvalResult(null)
      case Right(Result(r)) => DecisionEvalResult(r)
    }
  }

  post("/decisions") {
    val result = fileMultiParams.iterator.flatMap {
      case (name: String, files: Seq[FileItem]) => {
        files.map { file =>
          engine
            .insertDecisions(file.getInputStream, file.name)
            .map(_.map(decisionDto))
        }
      }
    }.toList

    val deployedDecisions = result.filter(_.isRight).flatMap(_.right.get)
    val failures = result.filter(_.isLeft).map(_.left.get)

    DeploymentResult(deployedDecisions, failures)
  }

  delete("/decisions/:resource") {
    val resource = params("resource")

    engine.removeResource(resource) match {
      case Left(failure)           => BadRequest(failure)
      case Right(removedDecisions) => removedDecisions.map(decisionDto)
    }
  }

  private def decisionDto(d: DeployedDecision) =
    Decision(d.decisionId, d.decisionName, d.resource)

}
