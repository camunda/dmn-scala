package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser._
import org.camunda.dmn.Audit._
import org.camunda.feel.syntaxtree.{ValBoolean, ValError, ValNull, ValNumber, ValString}
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

    auditLog.rootEntry.id should be("discount")
    auditLog.rootEntry.name should be("Discount")
    auditLog.rootEntry.decisionLogic shouldBe a[ParsedDecisionTable]
    auditLog.rootEntry.result shouldBe a[DecisionTableEvaluationResult]

    val result =
      auditLog.rootEntry.result.asInstanceOf[DecisionTableEvaluationResult]
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

    auditLog.requiredEntries.size should be(0)
  }

  it should "contains the result of a decision table if there is none" in {

    eval(discountDecision, "discount", Map("customer" -> "Other", "orderSize" -> 7))

    auditLog.rootEntry.id should be("discount")
    auditLog.rootEntry.name should be("Discount")
    auditLog.rootEntry.decisionLogic shouldBe a[ParsedDecisionTable]
    auditLog.rootEntry.result shouldBe a[DecisionTableEvaluationResult]

    val result = auditLog.rootEntry.result.asInstanceOf[DecisionTableEvaluationResult]
    result.inputs.size should be(2)

    result.inputs(0).input.id should be("input1")
    result.inputs(0).input.name should be("Customer")
    result.inputs(0).value should be(ValString("Other"))

    result.inputs(1).input.name should be("Order Size")
    result.inputs(1).value should be(ValNumber(7))

    result.matchedRules.size should be(0)

    result.result should be(ValNull)

    auditLog.requiredEntries.size should be(0)
  }

  it should "contains the result of a decision table if there is an exception" in {

    eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 9))

    val l = auditLog
    auditLog.rootEntry.id should be("discount")
    auditLog.rootEntry.name should be("Discount")
    auditLog.rootEntry.decisionLogic shouldBe a[ParsedDecisionTable]
    auditLog.rootEntry.result shouldBe a[DecisionTableEvaluationResult]

    val result = auditLog.rootEntry.result.asInstanceOf[DecisionTableEvaluationResult]
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

    result.result should be(ValError("multiple values aren't allowed for UNIQUE hit policy. found: 'List(Map(discount -> ValNumber(0.1)), Map(discount -> ValNumber(0.15)))'"))

    auditLog.requiredEntries.size should be(0)
  }

  it should "contains the result of a context" in {

    val variables = Map(
      "Applicant" -> Map("Age" -> 51, "Monthly" -> Map("Income" -> 10000.00)),
      "Affordability" -> Map("PreBureauRiskCategory" -> "DECLINE",
                             "InstallmentAffordable" -> true))

    eval(eligibilityContext, "eligibility", variables)

    auditLog.rootEntry.id should be("eligibility")
    auditLog.rootEntry.name should be("Eligibility")
    auditLog.rootEntry.decisionLogic shouldBe a[ParsedContext]
    auditLog.rootEntry.result shouldBe a[ContextEvaluationResult]

    val result = auditLog.rootEntry.result.asInstanceOf[ContextEvaluationResult]
    result.entries.size should be(4)

    result.entries("Age") should be(ValNumber(51))
    result.entries("MonthlyIncome") should be(ValNumber(10000.00))
    result.entries("PreBureauRiskCategory") should be(ValString("DECLINE"))
    result.entries("InstallmentAffordable") should be(ValBoolean(true))

    result.result should be(ValString("INELIGIBLE"))

    auditLog.requiredEntries.size should be(0)
  }

  it should "contains the result of an expression" in {

    eval(greeting, "greeting", Map("name" -> "John"))

    auditLog.rootEntry.id should be("greeting")
    auditLog.rootEntry.name should be("GreetingMessage")
    auditLog.rootEntry.decisionLogic shouldBe a[ParsedLiteralExpression]
    auditLog.rootEntry.result shouldBe a[SingleEvaluationResult]

    val result = auditLog.rootEntry.result.asInstanceOf[SingleEvaluationResult]
    result.result should be(ValString("Hello John"))

    auditLog.requiredEntries.size should be(0)
  }

  it should "contains the result of a BKM invocation" in {

    eval(discountBkm,
         "discount",
         Map("Customer" -> "Business", "OrderSize" -> 7))

    auditLog.rootEntry.id should be("discount")
    auditLog.rootEntry.name should be("Discount")
    auditLog.rootEntry.decisionLogic shouldBe a[ParsedInvocation]
    auditLog.rootEntry.result shouldBe a[SingleEvaluationResult]

    val result = auditLog.rootEntry.result.asInstanceOf[SingleEvaluationResult]
    result.result should be(ValNumber(0.1))

    auditLog.requiredEntries.size should be(1)
    auditLog.requiredEntries(0).id should be("bkm_discount")
    auditLog.requiredEntries(0).name should be("Discount table")
    auditLog.requiredEntries(0).decisionLogic shouldBe a[ParsedDecisionTable]
    auditLog.requiredEntries(0).result shouldBe a[DecisionTableEvaluationResult]
    auditLog.requiredEntries(0).result.result should be(ValNumber(0.1))
  }

  it should "contains the result of a BKM function invocation" in {

    eval(bkmFunction, "literalExpression", Map("x" -> 2, "y" -> 3))

    auditLog.rootEntry.id should be("literalExpression")
    auditLog.rootEntry.name should be("BKM with Literal Expression")
    auditLog.rootEntry.decisionLogic shouldBe a[ParsedLiteralExpression]
    auditLog.rootEntry.result shouldBe a[SingleEvaluationResult]

    val result = auditLog.rootEntry.result.asInstanceOf[SingleEvaluationResult]
    result.result should be(ValNumber(5))

    auditLog.requiredEntries.size should be(1)
    auditLog.requiredEntries(0).name should be("Sum")
    auditLog
      .requiredEntries(0)
      .decisionLogic shouldBe a[ParsedLiteralExpression]
    auditLog.requiredEntries(0).result shouldBe a[SingleEvaluationResult]
    auditLog.requiredEntries(0).result.result should be(ValNumber(5))
  }

}
