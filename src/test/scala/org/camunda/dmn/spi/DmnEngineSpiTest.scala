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
