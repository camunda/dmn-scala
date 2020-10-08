package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LiteralExpressionTest
    extends AnyFlatSpec
    with Matchers
    with DecisionTest {

  lazy val greeting = parse("/literalexpression/greeting.dmn")
  lazy val typeMismatch = parse("/literalexpression/type-mismatch.dmn")

  "A literal expression" should "be evaluated as decision" in {
    eval(greeting, "greeting", Map("name" -> "John")) should be(
      Result("Hello John"))
  }

  it should "fail when result doesn't match type" in {
    engine.eval(typeMismatch, "greeting", Map("name" -> "Frank")) should be(
      Left(Failure("expected 'number' but found 'ValString(Hello Frank)'")))
  }

}
