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

import org.camunda.bpm.model.dmn.{BuiltinAggregator, HitPolicy}
import org.camunda.dmn.Audit.DecisionTableEvaluationResult
import org.camunda.dmn.DmnEngine.EvalContext
import org.camunda.dmn.parser._
import org.camunda.feel.FeelEngine
import org.camunda.feel.syntaxtree.{ConstBool, ConstNull, If, ValNumber}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DecisionTableOptimizationTest extends AnyFlatSpec with Matchers
    with ConstantDecisionTableFixture {

  private def unary(text: String): ParsedExpression =
    FeelExpression(engine.feelEngine.parseUnaryTests(text).toEither.fold(
      failure => fail(failure.toString), identity))

  private def expression(text: String): ParsedExpression =
    FeelExpression(engine.feelEngine.parseExpression(text).toEither.fold(
      failure => fail(failure.toString), identity))

  private def table(tests: List[String], policy: HitPolicy,
                    aggregation: BuiltinAggregator = null): ParsedDecisionTable =
    ParsedDecisionTable(
      List(ParsedInput("input", "input", expression("value"))),
      List(ParsedOutput("output", "output", "output", None, None)),
      tests.zipWithIndex.map { case (text, i) =>
        ParsedRule(s"rule$i", List(unary(text)), List("output" -> constant(i % 2)))
      }, policy, aggregation)

  private def reference(table: ParsedDecisionTable): ParsedDecisionTable =
    table.copy(rules = table.rules.map(rule =>
      rule.copy(inputEntries = rule.inputEntries.map {
        case FeelExpression(parsed) =>
          FeelExpression(parsed.copy(expression =
            If(ConstBool(true), parsed.expression, ConstNull)))
        case other => other
      })))

  private def run(table: ParsedDecisionTable, variables: Map[String, Any]) = {
    val decision = ParsedDecision("decision", "decision", table, "output",
      None, Nil, Nil)
    val context = EvalContext(ParsedDmn(dummyModel, List(decision)),
      variables, decision)
    val result = new DecisionTableEvaluator(
      engine.literalExpressionEval.evalExpression).eval(table, context)
    val audit = context.auditLog.toList.map { entry =>
      val evaluated = entry.result.asInstanceOf[DecisionTableEvaluationResult]
      (evaluated.inputs.map(i => (i.input.id, i.value)),
        evaluated.matchedRules.map(r =>
          (r.rule.id, r.outputs.map(o => (o.output.id, o.value)))),
        evaluated.result)
    }
    (result, audit)
  }

  private def equivalent(table: ParsedDecisionTable,
                         variables: Map[String, Any]): Unit =
    run(table, variables) should be(run(reference(table), variables))

  "Optimized decision tables" should "preserve hit policies, rule order and audit values" in {
    for {
      policy <- List(HitPolicy.FIRST, HitPolicy.UNIQUE, HitPolicy.ANY,
        HitPolicy.RULE_ORDER, HitPolicy.COLLECT)
      value <- List[Any](1, 2, 3, 5, 8, null, "2", true)
    } {
      equivalent(table(List("1,2", "[1..5]", "> 7", "-"), policy),
        Map("value" -> value))
    }
    for (aggregation <- List(BuiltinAggregator.MIN, BuiltinAggregator.MAX,
      BuiltinAggregator.SUM, BuiltinAggregator.COUNT)) {
      equivalent(table(List("> 0", "< 5", "-"), HitPolicy.COLLECT, aggregation),
        Map("value" -> 2))
    }
  }

  it should "preserve null, missing inputs, dynamic tests and failures" in {
    val model = table(List("threshold", "< 5", "not(1,2)", "-"), HitPolicy.FIRST)
    for (variables <- List(Map[String, Any](), Map("value" -> null),
      Map("value" -> 3, "threshold" -> 3), Map("value" -> "a"),
      Map("value" -> 2, "threshold" -> 1))) {
      equivalent(model, variables)
    }
    equivalent(table(List("assert(false, false, \"failure\")", "-"),
      HitPolicy.FIRST), Map("value" -> 1))
  }

  it should "honor the overridden unary input variable" in {
    val variables = Map[String, Any]("value" -> 1, "alternate" -> ValNumber(8),
      FeelEngine.UnaryTests.inputVariable -> "alternate")
    equivalent(table(List("1", "8", "-"), HitPolicy.FIRST), variables)
    run(table(List("1", "8", "-"), HitPolicy.FIRST), variables)._1 should be(
      Right(ValNumber(1)))
  }
}
