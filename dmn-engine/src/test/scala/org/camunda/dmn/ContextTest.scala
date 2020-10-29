package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextTest extends AnyFlatSpec with Matchers with DecisionTest {

  lazy val simpleContext = parse("/context/SimpleContext.dmn")
  lazy val nestedContext = parse("/context/NestedContext.dmn")
  lazy val eligibilityContext = parse("/context/Eligibility.dmn")
  lazy val contextWithInvocation = parse("/context/ContextWithInvocation.dmn")

  "A context" should "return static values" in {
    eval(simpleContext, "applicantData", Map()) should be(
      Result(
        Map("Age" -> 51,
            "MaritalStatus" -> "M",
            "EmploymentStatus" -> "EMPLOYED",
            "ExistingCustomer" -> false)))
  }

  it should "invocate BKM" in {
    eval(contextWithInvocation,
         "discount",
         Map("Customer" -> "Business", "OrderSize" -> 7)) should be(
      Result(Map("Discount" -> 0.1, "ExistingCustomer" -> false)))
  }

  it should "return nested values" in {
    eval(nestedContext, "applicantData", Map()) should be(
      Result(
        Map("EmploymentStatus" -> "EMPLOYED",
            "Monthly" -> Map("Income" -> 10000.00,
                             "Repayments" -> 2500.00,
                             "Expenses" -> 3000.00))))
  }

  "A context with final result" should "return only final value" in {
    val variables = Map(
      "Applicant" -> Map("Age" -> 51, "Monthly" -> Map("Income" -> 10000.00)),
      "Affordability" -> Map("PreBureauRiskCategory" -> "DECLINE",
                             "InstallmentAffordable" -> true))

    eval(eligibilityContext, "eligibility", variables) should be(
      Result("INELIGIBLE"))
  }

}
