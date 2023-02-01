/*
 * Copyright © 2022 Camunda Services GmbH (info@camunda.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.{ExpressionFailure, ParsedLiteralExpression}
import org.camunda.feel.FeelEngineClock
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{ZoneId, ZonedDateTime}

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

  private val customClock = new FeelEngineClock {
    override def getCurrentTime: ZonedDateTime = ZonedDateTime.of(
      2021, 12, 21, 18, 9, 5, 333, ZoneId.of("Europe/Berlin"))
  }
  private val engineWithCustomClock = new DmnEngine(
    clock = customClock
  )

  private def decisionWithSpaces =
    getClass.getResourceAsStream("/config/decision_with_spaces.dmn")
  private def decisionWithSpacesInItemDefinitionAndItemComponents =
    getClass.getResourceAsStream("/config/decision_with_spaces_in_item_definition_and_item_component.dmn")
  private def decisionWithDash =
    getClass.getResourceAsStream("/config/decision_with_dash.dmn")
  private def bkmWithSpacesAndDash =
    getClass.getResourceAsStream("/config/bkm_with_spaces_and_dash.dmn")
  private def decisionWithInvalidExpression =
    getClass.getResourceAsStream("/config/decision_with_invalid_expression.dmn")
  private def decisionWithOtherInvalidDecision =
    getClass.getResourceAsStream("/config/with_invalid_decision.dmn")
  private def decisionWithInvalidBkm =
    getClass.getResourceAsStream("/config/with_invalid_bkm.dmn")
  private def decisionWithSpecificDateTime =
    getClass.getResourceAsStream("/config/with_specific_date_time.dmn")

  "A DMN engine with escaped names" should "evaluate a decision with spaces" in {

    val result = engineWithEscapeNames
      .parse(decisionWithSpaces)
      .flatMap(engineWithEscapeNames.eval(_, "greeting", Map("name" -> "DMN")))

    result.isRight should be(true)
    result.map(_.value should be("Hello DMN"))
  }

  "A DMN engine with escaped names" should "evaluate a decision with spaces in item definitions and item components" in {

    val result = engineWithEscapeNames
      .parse(decisionWithSpacesInItemDefinitionAndItemComponents)
      .flatMap(engineWithEscapeNames.eval(_, "greeting", Map("name" -> "Luke", "Last Name" -> "Skywalker")))

    result.isRight should be(true)
    result.map(_.value should be("Hello Luke Skywalker"))
  }

  it should "evaluate a decision with dash" in {

    val result = engineWithEscapeNames
      .parse(decisionWithDash)
      .flatMap(engineWithEscapeNames.eval(_, "greeting", Map("name" -> "DMN")))

    result.isRight should be(true)
    result.map(_.value should be("Hello DMN"))
  }

  it should "invoke a BKM with spaces & dash in parameter" in {
    val result = engineWithEscapeNames
      .parse(bkmWithSpacesAndDash)
      .flatMap(
        engineWithEscapeNames.eval(_,
                                   "greeting",
                                   Map("First Name" -> "Luke",
                                       "Last-Name" -> "Skywalker")))

    result.isRight should be(true)
    result.map(_.value should be("Hello Luke Skywalker"))
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

  "A DMN engine with custom clock" should "use that clock in decision" in {
    val result = engineWithCustomClock.parse(decisionWithSpecificDateTime)
      .flatMap(engineWithCustomClock.eval(_, "currentTime", Map.empty[String, Any]))

    result.isRight should be(true)
    result.map(_.value should be(customClock.getCurrentTime))
  }

}
