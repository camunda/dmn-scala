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

import org.camunda.bpm.model.dmn.HitPolicy
import org.camunda.dmn.DmnEngine.{EvalContext, Failure}
import org.camunda.dmn.parser._
import org.camunda.feel.FeelEngine
import org.camunda.feel.syntaxtree.{Ref, Val, ValBoolean, ValNull, ValNumber, ValString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InputEntryMemoizationTest extends AnyFlatSpec with Matchers
    with ConstantDecisionTableFixture {
  private def table(entry: ParsedExpression) = ParsedDecisionTable(
    List(ParsedInput("input", "input", constant(1))),
    List(ParsedOutput("output", "output", "output", None, None)),
    (0 until 3).map(i =>
      ParsedRule(s"rule$i", List(entry), List("output" -> constant(i)))).toList,
    HitPolicy.RULE_ORDER, null)

  "Memoization" should "reuse pure entries only within one evaluation" in {
    val model = table(constant(true))
    val evaluator = new ConstantExpressionEval
    evalDecisionTable(model, evaluator).isRight should be(true)
    evaluator.evaluatedInputEntries should be(1)
    evalDecisionTable(model, evaluator).isRight should be(true)
    evaluator.evaluatedInputEntries should be(2)
  }

  it should "evaluate variable-dependent entries on each visit" in {
    val entry = FeelExpression(org.camunda.feel.syntaxtree.ParsedExpression(
      Ref(List("x")), "x"))
    var count = 0
    val evaluator = new ConstantExpressionEval {
      override def apply(expression: ParsedExpression,
                         context: EvalContext): Either[Failure, Val] =
        if (expression eq entry) {
          count += 1
          Right(ValBoolean(true))
        } else super.apply(expression, context)
    }
    evalDecisionTable(table(entry), evaluator).isRight should be(true)
    count should be(3)
  }

  it should "cache false results without changing later rule visits" in {
    val evaluator = new ConstantExpressionEval
    evalDecisionTable(table(constant(false)), evaluator) should be(Right(ValNull))
    evaluator.evaluatedInputEntries should be(1)
  }

  it should "not cache null results" in {
    val entry = constant(true)
    var count = 0
    val evaluator = new ConstantExpressionEval {
      override def apply(expression: ParsedExpression,
                         context: EvalContext): Either[Failure, Val] =
        if (expression eq entry) {
          count += 1
          Right(ValNull)
        } else super.apply(expression, context)
    }
    evalDecisionTable(table(entry), evaluator) should be(Right(ValNull))
    count should be(3)
  }

  it should "stop at the first failure or invalid boolean result" in {
    val entry = constant(true)
    for (outcome <- List[Either[Failure, Val]](
      Left(Failure("failed input")), Right(ValString("invalid")))) {
      var count = 0
      val evaluator = new ConstantExpressionEval {
        override def apply(expression: ParsedExpression,
                           context: EvalContext): Either[Failure, Val] =
          if (expression eq entry) {
            count += 1
            outcome
          } else super.apply(expression, context)
      }
      evalDecisionTable(table(entry), evaluator).isLeft should be(true)
      count should be(1)
    }
  }

  it should "isolate identical expression instances in different columns" in {
    val entry = constant(true)
    val model = table(entry).copy(
      inputs = List(ParsedInput("a", "a", constant(1)),
        ParsedInput("b", "b", constant(2))),
      rules = table(entry).rules.map(_.copy(inputEntries = List(entry, entry))))
    var count = 0
    val evaluator = new ConstantExpressionEval {
      override def apply(expression: ParsedExpression,
                         context: EvalContext): Either[Failure, Val] =
        if (expression eq entry) {
          count += 1
          Right(ValBoolean(context.variables(
            FeelEngine.UnaryTests.defaultInputVariable) == ValNumber(1)))
        } else super.apply(expression, context)
    }
    evalDecisionTable(model, evaluator) should be(Right(ValNull))
    count should be(2)
  }

  it should "not reuse results when the unary input binding is overridden" in {
    val model = table(constant(true))
    val decision = ParsedDecision("decision", "decision", model, "output",
      None, Nil, Nil)
    val context = EvalContext(ParsedDmn(dummyModel, List(decision)),
      Map(FeelEngine.UnaryTests.inputVariable -> "alternate"), decision)
    val evaluator = new ConstantExpressionEval
    new DecisionTableEvaluator(evaluator).eval(model, context).isRight should be(true)
    evaluator.evaluatedInputEntries should be(3)
  }

  it should "exclude variable, function, time and nested dynamic expressions" in {
    for (text <- List("threshold", "date and time(\"2020-01-01T00:00:00\")",
      "now()", "1,threshold", "[1,threshold]", "not(threshold)",
      "< threshold", "[1..threshold]")) {
      val parsed = engine.feelEngine.parseUnaryTests(text).toEither.fold(
        failure => fail(failure.toString), identity)
      withClue(text) {
        PureUnaryTests.isPure(parsed.expression) should be(false)
      }
    }
  }

  it should "honor interruption even when the next entry is cached" in {
    val entry = constant(true)
    val evaluator = new ConstantExpressionEval {
      override def apply(expression: ParsedExpression,
                         context: EvalContext): Either[Failure, Val] =
        if (expression eq entry) {
          Thread.currentThread().interrupt()
          Right(ValBoolean(true))
        } else super.apply(expression, context)
    }
    try {
      an[InterruptedException] should be thrownBy {
        evalDecisionTable(table(entry), evaluator)
      }
    } finally {
      Thread.interrupted()
    }
  }
}
