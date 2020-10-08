package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import java.time.LocalDate

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionDefinitionTest
    extends AnyFlatSpec
    with Matchers
    with DecisionTest {

  lazy val applicantData = parse("/functionDefinition/ApplicantData.dmn")
  lazy val userFunction = parse("/functionDefinition/FeelUserFunction.dmn")
  lazy val contextWithFunction = parse(
    "/functionDefinition/ContextWithFunction.dmn")

  "A function definition" should "be invoked inside a context" in {
    val rate: BigDecimal = 0.25
    val term: BigDecimal = 36
    val amount: BigDecimal = 100000
    val expected = (amount * rate / 12) / (1 - (1 + rate / 12)
      .pow(-36)) // ~ 3975.982590125562

    eval(applicantData, "applicantData", Map()) should be(Result(expected))
  }

  "A FEEL user function" should "be invoked inside a context" in {
    eval(userFunction, "userFunction", Map()) should be(Result(5))
  }

  it should "be invoked outside of the context" in {
    eval(contextWithFunction, "calculation", Map()) should be(Result(5))
  }

}
