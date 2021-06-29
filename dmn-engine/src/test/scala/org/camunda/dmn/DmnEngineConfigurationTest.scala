package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnEngineConfigurationTest extends AnyFlatSpec with Matchers {

  private val engine = new DmnEngine(
    configuration = Configuration(
      escapeNamesWithSpaces = true,
      escapeNamesWithDashes = true
    ))

  private def decisionWithSpaces =
    getClass.getResourceAsStream("/config/decision_with_spaces.dmn")

  private def decisionWithDash =
    getClass.getResourceAsStream("/config/decision_with_dash.dmn")

  "The DMN engine" should "evaluate a decision with spaces" in {

    engine.eval(decisionWithSpaces, "greeting", Map("name" -> "DMN")) should be(
      Right(Result("Hello DMN")))
  }

  it should "evaluate a decision with dash" in {

    engine.eval(decisionWithDash, "greeting", Map("name" -> "DMN")) should be(
      Right(Result("Hello DMN")))
  }

}
