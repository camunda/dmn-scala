package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.ParsedDmn
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnEngineConfigurationTest extends AnyFlatSpec with Matchers {

  private val engine = new DmnEngine(
    configuration = Configuration(
      escapeNamesWithSpaces = true,
      escapeNamesWithDashes = true
    ))

  private def decisionWithSpaces = "/config/decision_with_spaces.dmn"

  private def decisionWithDash = "/config/decision_with_dash.dmn"

  private def parse(resourceName: String): ParsedDmn = {
    val resource = getClass.getResourceAsStream(resourceName)
    engine.parse(resource) match {
      case Right(parsedDmn) => parsedDmn
      case Left(failure)    => throw new AssertionError(failure)
    }
  }

  "The DMN engine" should "evaluate a decision with spaces" in {

    val parsedDmn = parse(decisionWithSpaces)
    val result =
      engine.eval(parsedDmn, "greeting", Map("name" -> "DMN"))

    result.isRight should be(true)
    result.map(_.value should be("Hello DMN"))
  }

  it should "evaluate a decision with dash" in {

    val parsedDmn = parse(decisionWithDash)
    val result = engine.eval(parsedDmn, "greeting", Map("name" -> "DMN"))

    result.isRight should be(true)
    result.map(_.value should be("Hello DMN"))
  }

}
