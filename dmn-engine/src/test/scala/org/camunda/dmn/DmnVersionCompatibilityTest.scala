package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnVersionCompatibilityTest extends AnyFlatSpec with Matchers {

  private val engine = new DmnEngine

  private def dmn1_1_decision =
    getClass.getResourceAsStream("/dmn1.1/greeting.dmn")

  private def dmn1_2_decision =
    getClass.getResourceAsStream("/dmn1.2/greeting.dmn")

  private def dmn1_3_decision =
    getClass.getResourceAsStream("/dmn1.2/greeting.dmn")

  "The DMN engine" should "evaluate a DMN 1.1 decision" in {

    engine.eval(dmn1_1_decision, "greeting", Map("name" -> "DMN")) should be(
      Right(Result("Hello DMN")))
  }

  it should "evaluate a DMN 1.2 decision" in {

    engine.eval(dmn1_2_decision, "greeting", Map("name" -> "DMN")) should be(
      Right(Result("Hello DMN")))
  }

  it should "evaluate a DMN 1.3 decision" in {

    engine.eval(dmn1_3_decision, "greeting", Map("name" -> "DMN")) should be(
      Right(Result("Hello DMN")))
  }

}
