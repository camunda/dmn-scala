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

class InvocationTest extends AnyFlatSpec with Matchers with DecisionTest {

  lazy val discountDecision = parse("/invocation/discount.dmn")
  lazy val withoutParameters = parse("/invocation/withoutParameters.dmn")
  lazy val missingParameter = parse("/invocation/missingParameters.dmn")
  lazy val missingKnowledgeRequirementDecision =
    getClass.getResourceAsStream("/invocation/missingKnowledgeRequirement.dmn")

  "An invocation" should "execute a BKM with parameters" in {
    eval(discountDecision,
         "discount",
         Map("Customer" -> "Business", "OrderSize" -> 7)) should be(0.1)
  }

  it should "execute a BKM without parameters" in {
    eval(withoutParameters, "applicantData", Map()) should be(
      Map("Age" -> 51,
          "MaritalStatus" -> "M",
          "EmploymentStatus" -> "EMPLOYED",
          "ExistingCustomer" -> false))
  }

  it should "fail if parameter is not set" in {
    eval(missingParameter, "discount", Map("OrderSize" -> 7)) should be(
      Failure("no parameter found with name 'customer'"))
  }

  it should "fail if parameter has the wrong type" in {
    eval(discountDecision,
         "discount",
         Map("Customer" -> "Business", "OrderSize" -> "foo")) should be(
      Failure("expected 'number' but found '\"foo\"'"))
  }

  it should "fail if knowledge requirement is missing" in {
    val result = engine.parse(missingKnowledgeRequirementDecision)

    result.isLeft should be(true)
    result.left.map(
      _.message should be("no BKM found with name 'Discount table'"))
  }

}
