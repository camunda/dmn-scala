package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnVersionCompatibilityTest
    extends AnyFlatSpec
    with Matchers
    with DecisionTest {

  private def dmn1_1_decision = parse("/dmn1.1/greeting.dmn")

  private def dmn1_2_decision = parse("/dmn1.2/greeting.dmn")

  private def dmn1_3_decision = parse("/dmn1.2/greeting.dmn")

  "The DMN engine" should "evaluate a DMN 1.1 decision" in {
    eval(dmn1_1_decision, "greeting", Map("name" -> "DMN")) should be(
      "Hello DMN")
  }

  it should "evaluate a DMN 1.2 decision" in {
    eval(dmn1_2_decision, "greeting", Map("name" -> "DMN")) should be(
      "Hello DMN")
  }

  it should "evaluate a DMN 1.3 decision" in {
    eval(dmn1_3_decision, "greeting", Map("name" -> "DMN")) should be(
      "Hello DMN")
  }

}
