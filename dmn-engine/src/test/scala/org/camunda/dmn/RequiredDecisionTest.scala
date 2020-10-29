package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RequiredDecisionTest extends AnyFlatSpec with Matchers with DecisionTest {

  lazy val discountDecision = parse("/requirements/discount.dmn")
  lazy val applicantDataDecision = parse("/requirements/ApplicantData.dmn")

  "A decision" should "evaluate a required decision" in {
    eval(discountDecision,
         "price",
         Map("customer" -> "Business", "orderSize" -> 7)) should be(Result(10))
  }

  it should "evaluate multiple required decision" in {
    eval(applicantDataDecision, "income", Map()) should be(Result(10000.00))
  }
}
