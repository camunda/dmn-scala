package org.camunda.dmn.zeebe

import org.camunda.dmn._

import scala.collection.JavaConversions._
import org.springframework.boot.autoconfigure.SpringBootApplication
import io.zeebe.spring.client.EnableZeebeClient
import org.json4s._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}
import org.json4s.jackson.JsonMethods._
import org.springframework.boot.SpringApplication
import io.zeebe.client.TasksClient
import io.zeebe.client.event.TaskEvent
import io.zeebe.spring.client.annotation.ZeebeTaskListener
import javax.annotation.PostConstruct
import org.camunda.dmn.standalone.StandaloneEngine
import org.camunda.dmn.DmnEngine._
import org.springframework.beans.factory.annotation.Value

object ZeebeDmnWorkerApplication {
  
  def main(args: Array[String]) {
    SpringApplication.run(classOf[ZeebeDmnWorkerApplication], args: _*)
  }
  
}

@SpringBootApplication
@EnableZeebeClient
class ZeebeDmnWorkerApplication {
  
  implicit val formats = DefaultFormats

  case class DecisionResult(result: Any)
  
  
  @Value("${dmn.repo:dmn-repo}")
  var repository: String = _
  
  lazy val engine = StandaloneEngine.fileSystemRepository(repository)
  
  @PostConstruct
  def init() 
  {
    // init the engine on startup
    
    val deployedDecisions = engine
      .getDecisions
      .map(d => s"${d.decisionId} (${d.resource})")
    
    logger.trace("Deployed decisions: {}", deployedDecisions)
  }
  
  @ZeebeTaskListener(taskType = "DMN")
  def handleDmnWorkItem(implicit client: TasksClient, task: TaskEvent) 
  {
    logger.trace("Received {}", task)
    
    Option(task.getCustomHeaders.get("decisionRef")).map(_.toString) match {
      case None              => error("missing task header 'decisionRef'")
      case Some(decisionId)  => 
      {
        val variables = Option(task.getPayload)
          .filterNot(s => s.isEmpty || s == "null")
          .map(p => parse(p).extract[Map[String, Any]])
          .getOrElse(Map.empty)
        
        evalDecision(decisionId, variables)
      }
    }    
  }
  
  private def evalDecision(decisionId: String, variables: Map[String, Any])(implicit client: TasksClient, task: TaskEvent)
  {
    engine.evalDecisionById(decisionId, variables) match {
      case Left(Failure(msg)) => error(s"Fail to evaluate decision '$decisionId': $msg")
      case Right(NilResult)   => completeTask(null)
      case Right(Result(r))   => completeTask(r)
    }
  }
  
  private def completeTask(result: Any) (implicit client: TasksClient, task: TaskEvent)
  {
    val json = write(DecisionResult(result))

    client.complete(task)
      .payload(json)
      .execute()
  }
  
  private def error(failure: String) 
  {
    // the exception mark the task as failed
    throw new RuntimeException(failure)
  }
  
}