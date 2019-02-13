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
import io.zeebe.test.ZeebeTestRule
import io.zeebe.model.bpmn.Bpmn
import java.util.Properties
import org.camunda.dmn.zeebe.ZeebeDmnWorkerApplication
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import org.junit.After
import java.io.ByteArrayInputStream
import io.zeebe.client.ZeebeClient
import io.zeebe.test.util.record.RecordingExporter
import io.zeebe.protocol.intent.JobIntent
import io.zeebe.exporter.record.value.job.Headers

class ZeebeDmnWorkerTest extends JUnitSuite with Matchers {

  val repository = Paths.get(getClass.getResource("/repository").toURI()).toString

  @(Rule @getter)
  val testRule = new ZeebeTestRule()

  var worker: ZeebeDmnWorkerApplication = _

  var client: ZeebeClient = _

  @Before def init {

    worker = new ZeebeDmnWorkerApplication(repository, testRule.getClient.getConfiguration.getBrokerContactPoint)

    Future {
      worker.start
    }

    val workflow = Bpmn.createExecutableProcess("wf")
      .startEvent()
      .serviceTask("dmn-task", t => t
        .zeebeTaskType("DMN")
        .zeebeTaskHeader("decisionRef", "discount"))
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
      .payload("""{"customer":"Business","orderSize":15}""")
      .send()
      .join()

    val jobRecord = RecordingExporter
      .jobRecords(JobIntent.COMPLETED)
      .getFirst

    jobRecord.getValue.getPayload should be("""{"result":0.15}""")
  }

  @Test def shouldReturnNilResult {

    val workflowInstance = client
      .newCreateInstanceCommand()
      .bpmnProcessId("wf")
      .latestVersion()
      .payload("""{"customer":"VIP","orderSize":100}""")
      .send()
      .join()

    val jobRecord = RecordingExporter
      .jobRecords(JobIntent.COMPLETED)
      .getFirst

    jobRecord.getValue.getPayload should be("""{"result":null}""")
  }
}
