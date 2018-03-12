package org.camunda.dmn.rest

import org.scalatra.test.scalatest._
import org.camunda.dmn.standalone.StandaloneEngine
import java.nio.file.Paths
import java.nio.file.Files
import java.io.File

class DmnRestServletTests extends ScalatraFunSuite {
  
  val repository = Paths.get(getClass.getResource("/repository").toURI()).toString  
  val engine = StandaloneEngine.fileSystemRepository(repository)
  val servlet = new DmnEngineRestServlet(engine)
  
  addServlet(servlet, "/*")

  test("GET /decisions on should return deployed decisions"){
    get("/decisions"){
      status should equal (200)
      body should equal ("""[{"id":"discount","name":"Discount","resource":"discount.dmn"}]""")
    }
  }
  
  test("POST /decisions/:id/eval on should return evaluation result"){
    post("/decisions/discount/eval", """{"customer":"Business","orderSize":15.0}"""){
      status should equal (200)
      body should equal ("""{"result":0.15}""")
    }
  }
  
  test("POST /decisions/:id/eval on should return evaluation failure"){
    post("/decisions/discount/eval", """{}"""){
      status should equal (400)
      body should equal ("""{"message":"failed to evaluate expression 'customer':\nno variable found for name 'customer'"}""")
    }
  }
  
  test("POST /decisions on should insert decision into repository"){
    val resource = new File(getClass.getResource("/repository2/adjustments.dmn").toURI())
    
    post("/decisions", params = List.empty, files = List(("adjustments.dmn", resource))){
      status should equal (200)
      body should equal ("""{"deployedDecisions":[{"id":"adjustments","name":"Adjustments","resource":"adjustments.dmn"}],"failures":[]}""")
    }
    
    get("/decisions"){
      status should equal (200)
      body should equal ("""[{"id":"discount","name":"Discount","resource":"discount.dmn"},{"id":"adjustments","name":"Adjustments","resource":"adjustments.dmn"}]""")
    }
  }
  
  test("DELETE /decisions/:resource on should remove decision from repository"){
    delete("/decisions/adjustments.dmn"){
      status should equal (200)
      body should equal ("""[{"id":"adjustments","name":"Adjustments","resource":"adjustments.dmn"}]""")
    }
    
    get("/decisions"){
      status should equal (200)
      body should equal ("""[{"id":"discount","name":"Discount","resource":"discount.dmn"}]""")
    }
  }

}
