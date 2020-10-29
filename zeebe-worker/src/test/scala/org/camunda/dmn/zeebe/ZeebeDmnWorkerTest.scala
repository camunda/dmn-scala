package org.camunda.dmn.standalone

import java.nio.file.Paths

import io.zeebe.client.ZeebeClient
import io.zeebe.model.bpmn.Bpmn
import io.zeebe.test.ZeebeTestRule
import io.zeebe.test.util.record.RecordingExporter
import org.camunda.dmn.zeebe.ZeebeDmnWorkerApplication
import org.junit.{After, Before, Rule, Test}
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.junit.JUnitSuite

import scala.annotation.meta.getter
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

    val workflow = Bpmn
      .createExecutableProcess("wf")
      .startEvent()
      .serviceTask("dmn-task")
      .zeebeJobType("DMN")
      .zeebeTaskHeader("decisionRef", "discount")
      .endEvent()
      .done()

    client = testRule.getClient

    client
      .newDeployCommand()
      .addWorkflowModel(workflow, "wf.bpmn")
      .send()
      .join()
  }

  @After def cleanUp {
    worker.stop

    RecordingExporter.reset()
  }

  @Test def shouldReturnDecisionResult {

    val workflowInstance = client
      .newCreateInstanceCommand()
      .bpmnProcessId("wf")
      .latestVersion()
      .variables("""{"customer":"Business","orderSize":15}""")
      .send()
      .join()

    ZeebeTestRule
      .assertThat(workflowInstance)
      .hasVariable("result", 0.15);
  }

  @Test def shouldReturnNilResult {

    val workflowInstance = client
      .newCreateInstanceCommand()
      .bpmnProcessId("wf")
      .latestVersion()
      .variables("""{"customer":"VIP","orderSize":100}""")
      .send()
      .join()

    ZeebeTestRule
      .assertThat(workflowInstance)
      .hasVariable("result", null);
  }
}
