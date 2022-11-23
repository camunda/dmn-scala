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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextTest extends AnyFlatSpec with Matchers with DecisionTest {

  lazy val simpleContext = parse("/context/SimpleContext.dmn")
  lazy val nestedContext = parse("/context/NestedContext.dmn")
  lazy val eligibilityContext = parse("/context/Eligibility.dmn")
  lazy val contextWithInvocation = parse("/context/ContextWithInvocation.dmn")

  "A context" should "return static values" in {
    eval(simpleContext, "applicantData", Map()) should be(
      Map("Age" -> 51,
          "MaritalStatus" -> "M",
          "EmploymentStatus" -> "EMPLOYED",
          "ExistingCustomer" -> false))
  }

  it should "invocate BKM" in {
    eval(contextWithInvocation,
         "discount",
         Map("Customer" -> "Business", "OrderSize" -> 7)) should be(
      Map("Discount" -> 0.1, "ExistingCustomer" -> false))
  }

  it should "return nested values" in {
    eval(nestedContext, "applicantData", Map()) should be(
      Map("EmploymentStatus" -> "EMPLOYED",
          "Monthly" -> Map("Income" -> 10000.00,
                           "Repayments" -> 2500.00,
                           "Expenses" -> 3000.00)))
  }

  "A context with final result" should "return only final value" in {
    val variables = Map(
      "Applicant" -> Map("Age" -> 51, "Monthly" -> Map("Income" -> 10000.00)),
      "Affordability" -> Map("PreBureauRiskCategory" -> "DECLINE",
                             "InstallmentAffordable" -> true))

    eval(eligibilityContext, "eligibility", variables) should be("INELIGIBLE")
  }

}
