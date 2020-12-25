package org.camunda.dmn.zeebe

import org.camunda.dmn._

import scala.collection.JavaConverters._
import org.camunda.dmn.standalone.StandaloneEngine
import org.camunda.dmn.DmnEngine._
import java.util.Collections

import io.zeebe.client.api.response.ActivatedJob
import io.zeebe.client.api.worker.{JobClient, JobHandler}

class DmnJobHandler(engine: StandaloneEngine) extends JobHandler {

  override def handle(client: JobClient, job: ActivatedJob) {
    Option(job.getCustomHeaders.get("decisionRef")).map(_.toString) match {
      case None => error("missing custom header 'decisionRef'")
      case Some(decisionId) => {
        val variables: Map[String, Any] = job.getVariablesAsMap.asScala.toMap

        engine.evalDecisionById(decisionId, variables) match {
          case Left(Failure(msg)) =>
            error(s"Fail to evaluate decision '$decisionId': $msg")
          case Right(NilResult(_))      => complete(client, job, null)
          case Right(Result(result, _)) => complete(client, job, result)
        }
      }
    }
  }

  private def complete(client: JobClient, job: ActivatedJob, result: Any) {
    client
      .newCompleteCommand(job.getKey)
      .variables(Collections.singletonMap("result", result))
      .send()
  }

  private def error(failure: String) {
    // the exception mark the job as failed
    throw new RuntimeException(failure)
  }

}
