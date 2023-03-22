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
import org.camunda.dmn.parser._
import org.camunda.dmn.Audit._
import org.camunda.feel.syntaxtree.{
  ValBoolean,
  ValError,
  ValNull,
  ValNumber,
  ValString
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AuditLogTest extends AnyFlatSpec with Matchers with DecisionTest {

  lazy val discountDecision = parse("/decisiontable/discount.dmn")
  lazy val eligibilityContext = parse("/context/Eligibility.dmn")
  lazy val greeting = parse("/literalexpression/greeting.dmn")
  lazy val discountBkm = parse("/invocation/discount.dmn")
  lazy val bkmFunction = parse("/bkm/BkmWithLiteralExpression.dmn")

  "The audit log" should "contains the result of a decision table" in {

    eval(discountDecision,
         "discount",
         Map("customer" -> "Business", "orderSize" -> 7))

    auditLog.entries should have size(1)

    val auditLogEntry = auditLog.entries.head
    auditLogEntry.id should be("discount")
    auditLogEntry.name should be("Discount")
    auditLogEntry.decisionLogic shouldBe a[ParsedDecisionTable]
    auditLogEntry.result shouldBe a[DecisionTableEvaluationResult]

    val result =
      auditLogEntry.result.asInstanceOf[DecisionTableEvaluationResult]
    result.inputs.size should be(2)

    result.inputs(0).input.id should be("input1")
    result.inputs(0).input.name should be("Customer")
    result.inputs(0).value should be(ValString("Business"))

    result.inputs(1).input.name should be("Order Size")
    result.inputs(1).value should be(ValNumber(7))

    result.matchedRules.size should be(1)
    result.matchedRules(0).outputs.size should be(1)
    result.matchedRules(0).outputs(0).output.name should be("discount")
    result.matchedRules(0).outputs(0).value should be(ValNumber(0.1))

    result.result should be(ValNumber(0.1))
  }

  it should "contains the result of a decision table if no rule matched" in {

    eval(discountDecision,
         "discount",
         Map("customer" -> "Other", "orderSize" -> 7))

    auditLog.entries should have size (1)

    val auditLogEntry = auditLog.entries.head
    auditLogEntry.id should be("discount")
    auditLogEntry.name should be("Discount")
    auditLogEntry.decisionLogic shouldBe a[ParsedDecisionTable]
    auditLogEntry.result shouldBe a[DecisionTableEvaluationResult]

    val result =
      auditLogEntry.result.asInstanceOf[DecisionTableEvaluationResult]
    result.inputs.size should be(2)

    result.inputs(0).input.id should be("input1")
    result.inputs(0).input.name should be("Customer")
    result.inputs(0).value should be(ValString("Other"))

    result.inputs(1).input.name should be("Order Size")
    result.inputs(1).value should be(ValNumber(7))

    result.matchedRules.size should be(0)

    result.result should be(ValNull)
  }

  it should "contains the result of a decision table if a failure occurred" in {

    eval(discountDecision,
         "discount",
         Map("customer" -> "Business", "orderSize" -> 9))

    auditLog.entries should have size (1)

    val auditLogEntry = auditLog.entries.head
    auditLogEntry.id should be("discount")
    auditLogEntry.name should be("Discount")
    auditLogEntry.decisionLogic shouldBe a[ParsedDecisionTable]
    auditLogEntry.result shouldBe a[DecisionTableEvaluationResult]

    val result =
      auditLogEntry.result.asInstanceOf[DecisionTableEvaluationResult]
    result.inputs.size should be(2)

    result.inputs(0).input.id should be("input1")
    result.inputs(0).input.name should be("Customer")
    result.inputs(0).value should be(ValString("Business"))

    result.inputs(1).input.name should be("Order Size")
    result.inputs(1).value should be(ValNumber(9))

    result.matchedRules.size should be(2)
    result.matchedRules(0).outputs.size should be(1)
    result.matchedRules(0).outputs(0).output.name should be("discount")
    result.matchedRules(0).outputs(0).value should be(ValNumber(0.1))

    result.result should be(ValError(
      "multiple values aren't allowed for UNIQUE hit policy. found: 'List(Map(discount -> ValNumber(0.1)), Map(discount -> ValNumber(0.15)))'"))
  }

  it should "contains the result of a context" in {

    val variables = Map(
      "Applicant" -> Map("Age" -> 51, "Monthly" -> Map("Income" -> 10000.00)),
      "Affordability" -> Map("PreBureauRiskCategory" -> "DECLINE",
                             "InstallmentAffordable" -> true))

    eval(eligibilityContext, "eligibility", variables)

    auditLog.entries should have size(1)

    val auditLogEntry = auditLog.entries.head
    auditLogEntry.id should be("eligibility")
    auditLogEntry.name should be("Eligibility")
    auditLogEntry.decisionLogic shouldBe a[ParsedContext]
    auditLogEntry.result shouldBe a[ContextEvaluationResult]

    val result = auditLogEntry.result.asInstanceOf[ContextEvaluationResult]
    result.entries.size should be(4)

    result.entries("Age") should be(ValNumber(51))
    result.entries("MonthlyIncome") should be(ValNumber(10000.00))
    result.entries("PreBureauRiskCategory") should be(ValString("DECLINE"))
    result.entries("InstallmentAffordable") should be(ValBoolean(true))

    result.result should be(ValString("INELIGIBLE"))
  }

  it should "contains the result of an expression" in {

    eval(greeting, "greeting", Map("name" -> "John"))

    auditLog.entries should have size(1)

    val auditLogEntry = auditLog.entries.head
    auditLogEntry.id should be("greeting")
    auditLogEntry.name should be("GreetingMessage")
    auditLogEntry.decisionLogic shouldBe a[ParsedLiteralExpression]
    auditLogEntry.result shouldBe a[SingleEvaluationResult]

    val result = auditLogEntry.result.asInstanceOf[SingleEvaluationResult]
    result.result should be(ValString("Hello John"))
  }

  it should "contains the result of a BKM invocation" in {

    eval(discountBkm,
         "discount",
         Map("Customer" -> "Business", "OrderSize" -> 7))

    auditLog.entries should have size(2)

    val bkmAuditLogEntry = auditLog.entries.head
    val rootAuditLogEntry = auditLog.entries.last

    rootAuditLogEntry.id should be("discount")
    rootAuditLogEntry.name should be("Discount")
    rootAuditLogEntry.decisionLogic shouldBe a[ParsedInvocation]
    rootAuditLogEntry.result shouldBe a[SingleEvaluationResult]

    val result = rootAuditLogEntry.result.asInstanceOf[SingleEvaluationResult]
    result.result should be(ValNumber(0.1))

    bkmAuditLogEntry.id should be("bkm_discount")
    bkmAuditLogEntry.name should be("Discount table")
    bkmAuditLogEntry.decisionLogic shouldBe a[ParsedDecisionTable]
    bkmAuditLogEntry.result shouldBe a[DecisionTableEvaluationResult]
    bkmAuditLogEntry.result.result should be(ValNumber(0.1))
  }

  it should "contains the result of a BKM function invocation" in {

    eval(bkmFunction, "literalExpression", Map("x" -> 2, "y" -> 3))

    auditLog.entries should have size (2)

    val bkmAuditLogEntry = auditLog.entries.head
    val rootAuditLogEntry = auditLog.entries.last

    rootAuditLogEntry.id should be("literalExpression")
    rootAuditLogEntry.name should be("BKM with Literal Expression")
    rootAuditLogEntry.decisionLogic shouldBe a[ParsedLiteralExpression]
    rootAuditLogEntry.result shouldBe a[SingleEvaluationResult]

    val result = rootAuditLogEntry.result.asInstanceOf[SingleEvaluationResult]
    result.result should be(ValNumber(5))

    bkmAuditLogEntry.name should be("Sum")
    bkmAuditLogEntry      .decisionLogic shouldBe a[ParsedLiteralExpression]
    bkmAuditLogEntry.result shouldBe a[SingleEvaluationResult]
    bkmAuditLogEntry.result.result should be(ValNumber(5))
  }

}
