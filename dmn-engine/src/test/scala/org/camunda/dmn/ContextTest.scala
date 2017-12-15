package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.camunda.dmn.DmnEngine._

class ContextTest extends FlatSpec with Matchers with DecisionTest {
  
  lazy val simpleContext = parse("/context/SimpleContext.dmn")
    
  "A context" should "return static values" in
  {
    eval(simpleContext, "applicantData", Map()) should be(
        Result(Map(
                   "Age" -> 51,
                   "MaritalStatus" -> "M", 
                   "EmploymentStatus" -> "EMPLOYED", 
                   "ExistingCustomer" -> false
                   ))) 
  }
  
}