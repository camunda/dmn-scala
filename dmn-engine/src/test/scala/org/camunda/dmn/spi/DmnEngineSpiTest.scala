package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnEngineSpiTest extends AnyFlatSpec with Matchers {

  val engine = new DmnEngine

  def decision = getClass.getResourceAsStream("/spi/SpiTests.dmn")

  "A custom value mapper" should "transform the input" in {

    engine.eval(decision, "varInput", Map("in" -> "bar")) should be(
      Right(Result("baz")))
  }

  it should "transform the output" in {

    engine.eval(decision, "varOutput", Map[String, Any]()) should be(
      Right(Result("baz")))
  }

  "A custom function provider" should "provide a function" in {

    engine.eval(decision, "invFunction", Map("x" -> 2)) should be(
      Right(Result(3)))
  }

}
