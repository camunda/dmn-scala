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

import org.camunda.dmn.DmnEngine.{EvalFailure, Failure}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DecisionServiceTest extends AnyFlatSpec with Matchers with DecisionTest {

  private lazy val pricing = parse("/decisionservice/pricing.dmn")

  "A decision service" should "be evaluated by id" in {
    val result = engine.evalDecisionService(pricing,
      "decision_service_pricing",
      Map("Amount" -> 200))

    result.isRight should be(true)
    result.map(_.value should be(240.0))
  }

  it should "be evaluated by name" in {
    val result = engine.evalDecisionServiceByName(pricing,
      "Pricing Service",
      Map("Amount" -> 50))

    result.isRight should be(true)
    result.map(_.value should be(55.0))
  }

  it should "be invocable from a decision" in {
    eval(pricing, "decision_internal_call", Map("BaseAmount" -> 50)) should be(55.0)
  }

  it should "return a failure when the id is unknown" in {
    val result = engine.evalDecisionService(pricing,
      "not-existing",
      Map("Amount" -> 10))

    result.isLeft should be(true)
    result.left.map {
      case EvalFailure(failure, _) =>
        failure should be(Failure("no decision service found with id 'not-existing'"))
    }
  }

  it should "return a failure when the name is unknown" in {
    val result = engine.evalDecisionServiceByName(pricing,
      "not-existing",
      Map("Amount" -> 10))

    result.isLeft should be(true)
    result.left.map {
      case EvalFailure(failure, _) =>
        failure should be(Failure("no decision service found with name 'not-existing'"))
    }
  }
}
