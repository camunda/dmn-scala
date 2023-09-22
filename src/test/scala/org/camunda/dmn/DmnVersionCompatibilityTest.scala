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
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnVersionCompatibilityTest
    extends AnyFlatSpec
    with Matchers
    with DecisionTest {

  private def dmn1_1_decision = parse("/dmn1.1/greeting.dmn")

  private def dmn1_2_decision = parse("/dmn1.2/greeting.dmn")

  private def dmn1_3_decision = parse("/dmn1.3/greeting.dmn")

  private def dmn1_4_decision = parse("/dmn1.4/greeting.dmn")

  private def dmn1_5_decision = parse("/dmn1.5/greeting.dmn")

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

  it should "evaluate a DMN 1.4 decision" in {
    eval(dmn1_4_decision, "greeting", Map("name" -> "DMN")) should be(
      "Hello DMN")
  }

  it should "evaluate a DMN 1.5 decision" in {
    eval(dmn1_4_decision, "greeting", Map("name" -> "DMN")) should be(
      "Hello DMN")
  }

}
