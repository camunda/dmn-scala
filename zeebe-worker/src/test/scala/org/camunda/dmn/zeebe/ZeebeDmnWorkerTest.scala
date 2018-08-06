package org.camunda.dmn.standalone

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.camunda.dmn.DmnEngine._
import java.nio.file.Paths
import org.scalatest.BeforeAndAfter
import java.nio.file.Files
import java.io.FileInputStream
import org.scalatest.junit.JUnitSuite
import scala.annotation.meta.getter
import org.junit.Before
import org.junit.Rule
import org.junit.Test
import io.zeebe.client.api.events.JobState
import io.zeebe.test.ZeebeTestRule
import io.zeebe.model.bpmn.Bpmn
import java.util.Properties
import io.zeebe.client.api.clients.TopicClient
import org.camunda.dmn.zeebe.ZeebeDmnWorkerApplication
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.junit.After
import java.io.ByteArrayInputStream

class ZeebeDmnWorkerTest extends JUnitSuite with Matchers {

  val repository = Paths.get(getClass.getResource("/repository").toURI()).toString

  @(Rule @getter)
  val testRule = new ZeebeTestRule()

  val worker = new ZeebeDmnWorkerApplication(repository)

  var client: TopicClient = _

  var payload: String = _

  @Before def init {

    payload = ""

    Future {
      worker.start
    }

    val workflow = Bpmn.createExecutableWorkflow("wf")
      .startEvent()
      .serviceTask("dmn-task", t => t
        .taskType("DMN")
        .taskHeader("decisionRef", "discount"))
      .endEvent()
      .done()

    client = testRule.getClient.topicClient

    client.workflowClient()
      .newDeployCommand()
      .addWorkflowModel(workflow, "wf.bpmn")
      .send()
      .join()

    // util to get the task result
    testRule.getClient()
      .topicClient()
      .newSubscription()
      .name("test")
      .jobEventHandler(job => {
        if (job.getState == JobState.COMPLETED) {
          payload = job.getPayload
        }
      })
      .open()
  }

  @After def cleanUp {
    worker.stop
  }

  @Test def shouldReturnDecisionResult {

    val workflowInstance = client.workflowClient().newCreateInstanceCommand()
      .bpmnProcessId("wf")
      .latestVersion()
      .payload("""{"customer":"Business","orderSize":15}""")
      .send()
      .join()

    testRule.waitUntilWorkflowInstanceCompleted(workflowInstance.getWorkflowInstanceKey())

    payload should be("""{"result":0.15}""")
  }

  @Test def shouldReturnNilResult {

    val workflowInstance = client.workflowClient().newCreateInstanceCommand()
      .bpmnProcessId("wf")
      .latestVersion()
      .payload("""{"customer":"VIP","orderSize":100}""")
      .send()
      .join()

    testRule.waitUntilWorkflowInstanceCompleted(workflowInstance.getWorkflowInstanceKey())

    payload should be("""{"result":null}""")
  }
}
