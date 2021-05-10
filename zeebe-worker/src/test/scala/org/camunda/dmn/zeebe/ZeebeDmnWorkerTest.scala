package org.camunda.dmn.standalone

import java.nio.file.Paths

import io.zeebe.client.ZeebeClient
import io.zeebe.model.bpmn.{Bpmn, BpmnModelInstance}
import io.zeebe.test.ZeebeTestRule
import io.zeebe.test.util.record.RecordingExporter
import org.camunda.dmn.zeebe.ZeebeDmnWorkerApplication
import org.junit.{After, Before, Rule, Test}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitSuite

import scala.annotation.meta.getter
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scala.jdk.CollectionConverters._

class ZeebeDmnWorkerTest extends JUnitSuite with Matchers {

  val repository =
    Paths.get(getClass.getResource("/repository").toURI()).toString

  @(Rule @getter)
  val testRule = new ZeebeTestRule()

  var worker: ZeebeDmnWorkerApplication = _

  var client: ZeebeClient = _

  @Before def init {

    worker = new ZeebeDmnWorkerApplication(
      repository,
      testRule.getClient.getConfiguration.getBrokerContactPoint)

    Future {
      worker.start
    }

    client = testRule.getClient

    client
      .newDeployCommand()
      .addWorkflowModel(workflow(processId = "discount", "discount"), "discount.bpmn")
      .addWorkflowModel(workflow(processId = "holidays", decisionRef = "holidays"), "holiday.bpmn")
      .addWorkflowModel(workflow(processId = "adjustments", decisionRef = "adjustments"), "adjustments.bpmn")
      .send()
      .join()
  }

  private def workflow(processId: String, decisionRef: String): BpmnModelInstance = Bpmn
    .createExecutableProcess(processId)
    .startEvent()
    .serviceTask("dmn-task")
    .zeebeJobType("DMN")
    .zeebeTaskHeader("decisionRef", decisionRef)
    .endEvent()
    .done()

  @After def cleanUp {
    worker.stop

    RecordingExporter.reset()
  }

  @Test def shouldReturnDecisionResult {

    val workflowInstance = client
      .newCreateInstanceCommand()
      .bpmnProcessId("discount")
      .latestVersion()
      .variables("""{"customer":"Business","orderSize":15}""")
      .send()
      .join()

    ZeebeTestRule
      .assertThat(workflowInstance)
      .hasVariable("result", 0.15)
  }

  @Test def shouldReturnNilResult {

    val workflowInstance = client
      .newCreateInstanceCommand()
      .bpmnProcessId("discount")
      .latestVersion()
      .variables("""{"customer":"VIP","orderSize":100}""")
      .send()
      .join()

    ZeebeTestRule
      .assertThat(workflowInstance)
      .hasVariable("result", null)
  }

  @Test def shouldReturnListResult {

    val workflowInstance = client
      .newCreateInstanceCommand()
      .bpmnProcessId("holidays")
      .latestVersion()
      .variables("""{"age":58,"yearsOfService":31}""")
      .send()
      .join()

    ZeebeTestRule
      .assertThat(workflowInstance)
      .hasVariable("result", List(22, 5, 3).asJava)
  }

  @Test def shouldReturnContextResult {

    val workflowInstance = client
      .newCreateInstanceCommand()
      .bpmnProcessId("adjustments")
      .latestVersion()
      .variables("""{"customer":"Business","orderSize":7}""")
     .send()
      .join()

    ZeebeTestRule
      .assertThat(workflowInstance)
      .hasVariable("result", Map("discount" -> 0.1, "shipping" -> "Air").asJava)
  }
}
