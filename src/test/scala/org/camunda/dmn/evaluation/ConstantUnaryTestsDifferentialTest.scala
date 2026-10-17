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
package org.camunda.dmn.evaluation

import org.camunda.feel.api.{FeelEngineBuilder, SuccessfulEvaluationResult}
import org.camunda.feel.context.Context.StaticContext
import org.camunda.feel.syntaxtree._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConstantUnaryTestsDifferentialTest extends AnyFlatSpec with Matchers {
  private val engine = FeelEngineBuilder().build()

  "Constant unary tests" should "agree with FEEL whenever the fast path answers" in {
    val texts = List("\"a\"", "\"\"", "\"with spaces\"", "5", "5.0", "5.5", "-5", "true", "[1,2,3]",
      "\"a\",\"b\"", "1,\"a\"", "[\"a\",1]", "< 5", "<= 5", "> 5", ">= 5",
      "[1..5]", "(1..5)", "[1..5)", "(1..5]", "not(5)", "not(< 3)",
      "not(\"a\",\"b\")", "< 3, > 7", "[5..1]", "[1..1]", "(1..1)",
      "999999999999999999999999999999999999", "< 0.00000000000000001",
      "-", "null", "x", "[1,null]", "[]", "1,x", "x,1",
      "date(\"2020-01-01\")", "? = 5")
    val inputs: List[Val] = List(ValNull, ValNumber(0), ValNumber(1),
      ValNumber(5), ValNumber(5.5), ValNumber(7), ValString("a"),
      ValString("b"), ValString("5"), ValString(""), ValString("with spaces"),
      ValBoolean(true), ValBoolean(false), ValNumber(-5),
      ValNumber(BigDecimal("999999999999999999999999999999999999")),
      ValNumber(BigDecimal("0.000000000000000001")),
      ValList(List(ValNumber(1))), ValError("input error"))
    var answered = 0
    for (text <- texts; input <- inputs) {
      val parsed = engine.parseUnaryTests(text).toEither.fold(
        failure => fail(failure.toString), identity)
      ConstantUnaryTests.evaluate(parsed.expression, input).foreach { result =>
        answered += 1
        withClue(s"$text / $input: ") {
          engine.evaluate(parsed, StaticContext(Map(
            org.camunda.feel.FeelEngine.UnaryTests.defaultInputVariable -> input))) match {
            case SuccessfulEvaluationResult(value, _) =>
              assert(value == result)
            case other => fail(other.toString)
          }
        }
      }
    }
    answered should be > 0
  }

  it should "fall back for null, incompatible types and dynamic expressions" in {
    for ((text, input) <- List(("5", ValNull), ("\"a\"", ValNumber(1)),
      ("< 5", ValString("a")), ("[1..5]", ValNull), ("x", ValNumber(1)),
      ("-", ValNumber(1)), ("1,x", ValNumber(1)))) {
      val parsed = engine.parseUnaryTests(text).toEither.fold(
        failure => fail(failure.toString), identity)
      ConstantUnaryTests.evaluate(parsed.expression, input) should be(None)
    }
  }

  it should "fall back when any list or alternative contains a dynamic expression" in {
    for (text <- List("[1,x]", "1,x", "x,1", "not(1,x)", "[1,null]")) {
      val parsed = engine.parseUnaryTests(text).toEither.fold(
        failure => fail(failure.toString), identity)
      ConstantUnaryTests.evaluate(parsed.expression, ValNumber(1)) should be(None)
    }
  }
}
