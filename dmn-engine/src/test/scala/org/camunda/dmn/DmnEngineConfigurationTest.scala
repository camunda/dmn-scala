package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.{
  ExpressionFailure,
  ParsedDmn,
  ParsedLiteralExpression
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnEngineConfigurationTest extends AnyFlatSpec with Matchers {

  private val engineWithEscapeNames = new DmnEngine(
    configuration = Configuration(
      escapeNamesWithSpaces = true,
      escapeNamesWithDashes = true
    ))

  private val engineWithLazyEvaluation = new DmnEngine(
    configuration = Configuration(
      lazyEvaluation = true
    ))

  private def decisionWithSpaces =
    getClass.getResourceAsStream("/config/decision_with_spaces.dmn")
  private def decisionWithDash =
    getClass.getResourceAsStream("/config/decision_with_dash.dmn")
  private def decisionWithInvalidExpression =
    getClass.getResourceAsStream("/config/decision_with_invalid_expression.dmn")
  private def decisionWithOtherInvalidDecision =
    getClass.getResourceAsStream("/config/with_invalid_decision.dmn")
  private def decisionWithInvalidBkm =
    getClass.getResourceAsStream("/config/with_invalid_bkm.dmn")

  "A DMN engine with escaped names" should "evaluate a decision with spaces" in {

    val result = engineWithEscapeNames
      .parse(decisionWithSpaces)
      .flatMap(engineWithEscapeNames.eval(_, "greeting", Map("name" -> "DMN")))

    result.isRight should be(true)
    result.map(_.value should be("Hello DMN"))
  }

  it should "evaluate a decision with dash" in {

    val result = engineWithEscapeNames
      .parse(decisionWithDash)
      .flatMap(engineWithEscapeNames.eval(_, "greeting", Map("name" -> "DMN")))

    result.isRight should be(true)
    result.map(_.value should be("Hello DMN"))
  }

  "A DMN engine with lazy evaluation" should "ignore invalid expression on parsing" in {

    val result = engineWithLazyEvaluation.parse(decisionWithInvalidExpression)

    result.isRight should be(true)
    result.map(_.decisionsById("greeting").logic match {
      case ParsedLiteralExpression(expression) =>
        expression shouldBe a[ExpressionFailure]
    })
  }

  it should "report failure on evaluation" in {

    val result = engineWithLazyEvaluation
      .parse(decisionWithInvalidExpression)
      .flatMap(
        engineWithLazyEvaluation.eval(_, "greeting", Map("name" -> "DMN")))

    result.isRight should be(false)
    result.left.map {
      case EvalFailure(Failure(failure), _) =>
        failure should startWith("FEEL expression: failed to parse expression")
    }
  }

  it should "ignore other invalid decision" in {
    val result = engineWithLazyEvaluation
      .parse(decisionWithOtherInvalidDecision)
      .flatMap(
        engineWithLazyEvaluation.eval(_, "greeting", Map("name" -> "DMN")))

    result.isRight should be(true)
    result.map(_.value should be("Hello DMN"))
  }

  it should "ignore other invalid BKM" in {
    val result = engineWithLazyEvaluation
      .parse(decisionWithInvalidBkm)
      .flatMap(
        engineWithLazyEvaluation.eval(_, "greeting", Map("name" -> "DMN")))

    result.isRight should be(true)
    result.map(_.value should be("Hello DMN"))
  }

}
