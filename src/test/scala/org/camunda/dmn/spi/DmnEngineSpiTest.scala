package org.camunda.dmn.spi

import org.camunda.dmn.DmnEngine
import org.camunda.dmn.parser.ParsedDmn
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnEngineSpiTest extends AnyFlatSpec with Matchers {

  val engine = new DmnEngine

  lazy val decision: ParsedDmn = {
    val resource = getClass.getResourceAsStream("/spi/SpiTests.dmn")
    engine.parse(resource) match {
      case Right(decision) => decision
      case Left(failure)   => throw new AssertionError(failure)
    }
  }

  "A custom value mapper" should "transform the input" in {

    val result = engine.eval(decision, "varInput", Map("input" -> "bar"))

    result.isRight should be(true)
    result.map(_.value should be("baz"))
  }

  it should "transform the output" in {

    val result = engine.eval(decision, "varOutput", Map[String, Any]())

    result.isRight should be(true)
    result.map(_.value should be("baz"))
  }

  "A custom function provider" should "provide a function" in {

    val result = engine.eval(decision, "invFunction", Map("x" -> 2))

    result.isRight should be(true)
    result.map(_.value should be(3))
  }

}
