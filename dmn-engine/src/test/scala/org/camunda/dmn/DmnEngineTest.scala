package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.ParsedDmn
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.InputStream

class DmnEngineTest extends AnyFlatSpec with Matchers {

  private val engine = new DmnEngine

  private def discountDecision =
    getClass.getResourceAsStream("/decisiontable/discount.dmn")
  private def invalidExpressionDecision =
    getClass.getResourceAsStream("/decisiontable/invalid-expression.dmn")
  private def expressionLanguageDecision =
    getClass.getResourceAsStream("/decisiontable/expression-language.dmn")
  private def emptyExpressionDecision =
    getClass.getResourceAsStream("/decisiontable/empty-expression.dmn")

  private def parse(resource: InputStream): ParsedDmn = {
    engine.parse(resource) match {
      case Right(decision) => decision
      case Left(failure)   => throw new AssertionError(failure)
    }
  }

  "A DMN engine" should "evaluate a decision table" in {

    val parsedDmn = parse(discountDecision)
    val result = engine.eval(parsedDmn,
                             "discount",
                             Map("customer" -> "Business", "orderSize" -> 7))

    result.isRight should be(true)
    result.map(_.value should be(0.1))
  }

  it should "parse and evaluate a decision table" in {

    val parseResult = engine.parse(discountDecision)
    parseResult.isRight should be(true)

    parseResult.map { parsedDmn =>
      val result = engine.eval(parsedDmn,
                               "discount",
                               Map("customer" -> "Business", "orderSize" -> 7))

      result.isRight should be(true)
      result.map(_.value should be(0.1))
    }
  }

  it should "report parse failures" in {

    val parseResult = engine.parse(invalidExpressionDecision)

    parseResult.isLeft should be(true)
    parseResult.left.map(
      _.message should include("Failed to parse FEEL unary-tests '> 10L'"))
  }

  it should "report parse failures on evaluation" in {

    val result =
      engine.eval(invalidExpressionDecision, "discount", Map[String, Any]())

    result.isLeft should be(true)
    result.left.map(
      _.message should include("Failed to parse FEEL unary-tests '> 10L'"))
  }

  it should "report an evaluation failure" in {

    val parsedDmn = parse(discountDecision)
    val result = engine.eval(
      parsedDmn,
      "discount",
      Map[String, Any]("customer" -> "Business", "orderSize" -> "foo"))

    result.isLeft should be(true)
    result.left.map(
      _.failure.message should include("failed to evaluate expression '< 10'"))
  }

  it should "report parse failures if expression language is set" in {

    val parseResult = engine.parse(expressionLanguageDecision)

    parseResult.isLeft should be(true)
    parseResult.left.map(
      _.message should include("Expression language 'groovy' is not supported"))
  }

  it should "report parse failures if an expression has no content" in {

    val parseResult = engine.parse(emptyExpressionDecision)

    parseResult.isLeft should be(true)

    val failure = parseResult.left.get

    failure.message should include(
      "The expression 'inputExpression1' must not be empty.")
  }

  it should "evaluate a decision and return the audit log" in {
    val parsedDmn = parse(discountDecision)
    val result = engine.eval(parsedDmn,
                             "discount",
                             Map("customer" -> "Business", "orderSize" -> 7))

    result.isRight should be(true)
    result.map {
      case Result(value, auditLog) =>
        value should be(0.1)

        auditLog.dmn should be(parsedDmn)
        auditLog.rootEntry.id should be("discount")
        auditLog.rootEntry.name should be("Discount")
    }
  }

  it should "report an evaluation failure and return the audit log" in {
    val parsedDmn = parse(discountDecision)
    val result =
      engine.eval(parsedDmn,
                  "discount",
                  Map("customer" -> "Business", "orderSize" -> "foo"))

    result.isLeft should be(true)
    result.left.map {
      case EvalFailure(failure, auditLog) =>
        failure.message should include("failed to evaluate expression '< 10'")

        auditLog.dmn should be(parsedDmn)
        auditLog.rootEntry.id should be("discount")
        auditLog.rootEntry.name should be("Discount")
    }
  }

}
