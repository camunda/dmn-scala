package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InvocationTest extends AnyFlatSpec with Matchers with DecisionTest {

  lazy val discountDecision = parse("/invocation/discount.dmn")
  lazy val withoutParameters = parse("/invocation/withoutParameters.dmn")
  lazy val missingParameter = parse("/invocation/missingParameters.dmn")
  lazy val missingKnowledgeRequirementDecision =
    getClass.getResourceAsStream("/invocation/missingKnowledgeRequirement.dmn")

  "An invocation" should "execute a BKM with parameters" in {
    eval(discountDecision,
         "discount",
         Map("Customer" -> "Business", "OrderSize" -> 7)) should be(0.1)
  }

  it should "execute a BKM without parameters" in {
    eval(withoutParameters, "applicantData", Map()) should be(
      Map("Age" -> 51,
          "MaritalStatus" -> "M",
          "EmploymentStatus" -> "EMPLOYED",
          "ExistingCustomer" -> false))
  }

  it should "fail if parameter is not set" in {
    eval(missingParameter, "discount", Map("OrderSize" -> 7)) should be(
      Failure("no parameter found with name 'customer'"))
  }

  it should "fail if parameter has the wrong type" in {
    eval(discountDecision,
         "discount",
         Map("Customer" -> "Business", "OrderSize" -> "foo")) should be(
      Failure("expected 'number' but found 'ValString(foo)'"))
  }

  it should "fail if knowledge requirement is missing" in {
    val result = engine.parse(missingKnowledgeRequirementDecision)

    result.isLeft should be(true)
    result.left.map(
      _.message should be("no BKM found with name 'Discount table'"))
  }

}
