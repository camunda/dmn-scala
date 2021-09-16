package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import java.time.LocalDate

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionDefinitionTest
    extends AnyFlatSpec
    with Matchers
    with DecisionTest {

  private lazy val applicantData = parse("/functionDefinition/ApplicantData.dmn")
  private lazy val userFunction = parse("/functionDefinition/FeelUserFunction.dmn")
  private lazy val requiredDecisionWithFunction = parse(
    "/functionDefinition/RequiredDecisionWithFunction.dmn")
  private lazy val requiredDecisionWithContext = parse(
    "/functionDefinition/RequiredDecisionWithContext.dmn")
  private lazy val requiredBkmWithFunction = parse(
    "/functionDefinition/RequiredBkmWithFunction.dmn")
  private lazy val requiredBkmWithContext = parse(
    "/functionDefinition/RequiredBkmWithContext.dmn")

  "A function definition" should "be invoked inside a context" in {
    val rate: BigDecimal = 0.25
    val term: BigDecimal = 36
    val amount: BigDecimal = 100000
    val expected = (amount * rate / 12) / (1 - (1 + rate / 12)
      .pow(-36)) // ~ 3975.982590125562

    eval(applicantData, "applicantData", Map()) should be(expected)
  }

  "A FEEL user function" should "be invoked inside a context" in {
    eval(userFunction, "userFunction", Map()) should be(5)
  }

  it should "be invoked from required decision with function" in {
    eval(requiredDecisionWithFunction, "calculation", Map()) should be(5)
  }

  it should "be invoked from required decision with context" in {
    eval(requiredDecisionWithContext, "calculation", Map()) should be(5)
  }

  // TODO (saig0): support invoking required BKM as functions (#12)
  ignore should "be invoked from required BKM with function" in {
    eval(requiredBkmWithFunction, "calculation", Map()) should be(5)
  }

  // TODO (saig0): support invoking required BKM as functions (#12)
  ignore should "be invoked from required BKM with context" in {
    eval(requiredBkmWithContext, "calculation", Map()) should be(5)
  }


}
