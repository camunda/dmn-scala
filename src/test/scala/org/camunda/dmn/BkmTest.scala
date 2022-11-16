package org.camunda.dmn

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BkmTest extends AnyFlatSpec with Matchers with DecisionTest {

  lazy val literalExpression = parse("/bkm/BkmWithLiteralExpression.dmn")
  lazy val context = parse("/bkm/BkmWithContext.dmn")
  lazy val relation = parse("/bkm/BkmWithRelation.dmn")
  lazy val decisionTable = parse("/bkm/BkmWithDecisionTable.dmn")
  lazy val withoutEncapsulatedLogic = parse(
    "/bkm/BkmWithoutEncapsulatedLogic.dmn")

  "A BKM with a Literal Expression" should "be invoked as function" in {
    eval(literalExpression, "literalExpression", Map("x" -> 2, "y" -> 3)) should be(
      5)
  }

  "A BKM with a Context" should "be invoked as function" in {
    eval(context, "context", Map("x" -> 2, "y" -> 3)) should be(
      Map(
        "Sum" -> 5,
        "Multiply" -> 6
      ))
  }

  "A BKM with a Relation" should "be invoked as function" in {
    eval(relation, "relation", Map("x" -> 2, "y" -> 3)) should be(
      List(
        Map("rate" -> "A", "fee" -> 5),
        Map("rate" -> "B", "fee" -> 7.5),
        Map("rate" -> "C", "fee" -> 8.75)
      ))
  }

  "A BKM with a Decision Table" should "be invoked as function" in {
    eval(decisionTable, "decisionTable", Map("x" -> "Business", "y" -> 7)) should be(
      0.1)
  }

  "A BKM without encapsulated logic" should "be ignored" in {
    eval(withoutEncapsulatedLogic, "literalExpression", Map("x" -> 2, "y" -> 3)) should be(
      5)
  }

}
