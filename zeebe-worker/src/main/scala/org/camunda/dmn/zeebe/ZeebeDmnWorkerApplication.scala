package org.camunda.dmn.zeebe

import org.camunda.dmn._

import org.camunda.dmn.standalone.StandaloneEngine
import org.camunda.dmn.DmnEngine._
import io.zeebe.client.ZeebeClient
import java.time.Duration
import java.util.Scanner
import java.util.concurrent.CountDownLatch

object ZeebeDmnWorkerApplication {

  def main(args: Array[String]) {

    val repository: String =
      Option(System.getenv("dmn.repo")).getOrElse("dmn-repo")

    val app = new ZeebeDmnWorkerApplication(repository)

    sys.addShutdownHook(app.stop)

    app.start
  }

}

class ZeebeDmnWorkerApplication(repository: String) {

  var openLatch = new CountDownLatch(1)

  lazy val zeebeClient: ZeebeClient = {

    val builder = ZeebeClient
      .newClientBuilder()
      .defaultJobWorkerName("script-worker")
      .defaultJobTimeout(Duration.ofSeconds(10))

    Option(System.getenv("zeebe.client.broker.contactPoint"))
      .map(builder.brokerContactPoint)
    Option(System.getenv("zeebe.client.topic")).map(builder.defaultTopic)

    builder.build
  }

  lazy val dmnEngine = StandaloneEngine.fileSystemRepository(repository)

  def start {
    logger.info("start Zeebe worker")

    val deployedDecisions = dmnEngine.getDecisions
      .map(d => s"${d.decisionId} (${d.resource})")

    logger.info(s"deployed decisions: $deployedDecisions")

    val handler = new DmnJobHandler(dmnEngine)
    zeebeClient
      .topicClient()
      .jobClient()
      .newWorker()
      .jobType("DMN")
      .handler(handler)
      .open()

    openLatch.await();
  }

  def stop {
    if (openLatch.getCount == 1) {
      openLatch.countDown

      logger.info("stop Zeebe worker")
      zeebeClient.close()
    }
  }

}
