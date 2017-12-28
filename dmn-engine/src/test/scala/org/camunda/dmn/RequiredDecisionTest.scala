package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.camunda.dmn.DmnEngine._

class RequiredDecisionTest extends FlatSpec with Matchers with DecisionTest {
  
  lazy val discountDecision = parse("/requirements/discount.dmn")
  lazy val applicantDataDecision = parse("/requirements/ApplicantData.dmn")
  
  "A decision" should "evaluate a required decision" in
  {
    eval(discountDecision, "price", Map("customer" -> "Business", "orderSize" -> 7)) should be(Result(10)) 
  }
  
  it should "evaluate multiple required decision" in 
  {
    eval(applicantDataDecision, "income", Map()) should be(Result(10000.00))
  }
}