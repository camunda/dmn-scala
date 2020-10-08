package org.camunda.dmn.rest

import java.io.File
import java.nio.file.Paths

import org.camunda.dmn.standalone.StandaloneEngine
import org.scalatra.test.scalatest.ScalatraFlatSpec

class DmnRestServletTests extends ScalatraFlatSpec {

  val repository =
    Paths.get(getClass.getResource("/repository").toURI()).toString
  val engine = StandaloneEngine.fileSystemRepository(repository)
  val servlet = new DmnEngineRestServlet(engine)

  addServlet(servlet, "/*")

  override def header = ???

  "A GET /decsions" should "return deployed decisions" in {
    get("/decisions") {
      status should be(200)
      body should equal(
        """[{"id":"discount","name":"Discount","resource":"discount.dmn"}]""")
    }
  }

  "A POST /decisions/:id/eval" should "return evaluation result" in {
    post("/decisions/discount/eval",
         """{"customer":"Business","orderSize":15.0}""") {
      status should equal(200)
      body should equal("""{"result":0.15}""")
    }
  }

  it should "return evaluation failure" in {
    post("/decisions/discount/eval", """{}""") {
      status should equal(400)
      body should equal(
        """{"message":"failed to evaluate expression 'customer': no variable found for name 'customer'"}""")
    }
  }

  "A POST /decisions" should "insert decision into repository" in {
    val resource =
      new File(getClass.getResource("/repository2/adjustments.dmn").toURI())

    post("/decisions",
         params = List.empty,
         files = List(("adjustments.dmn", resource))) {
      status should equal(200)
      body should equal(
        """{"deployedDecisions":[{"id":"adjustments","name":"Adjustments","resource":"adjustments.dmn"}],"failures":[]}""")
    }

    get("/decisions") {
      status should equal(200)
      body should equal(
        """[{"id":"adjustments","name":"Adjustments","resource":"adjustments.dmn"},{"id":"discount","name":"Discount","resource":"discount.dmn"}]""")
    }
  }

  "A DELETE /decisions/:resource" should "remove decision from repository" in {
    delete("/decisions/adjustments.dmn") {
      status should equal(200)
      body should equal(
        """[{"id":"adjustments","name":"Adjustments","resource":"adjustments.dmn"}]""")
    }

    get("/decisions") {
      status should equal(200)
      body should equal(
        """[{"id":"discount","name":"Discount","resource":"discount.dmn"}]""")
    }
  }

}
