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

  // Like the parser, reuse one parsed entry per distinct text so that
  // repeated conditions share the same instance (the memoization key).
  private def table(tests: List[String], policy: HitPolicy,
                    aggregation: BuiltinAggregator = null): ParsedDecisionTable = {
    val entries = scala.collection.mutable.Map[String, ParsedExpression]()
    ParsedDecisionTable(
      List(ParsedInput("input", "input", expression("value"))),
      List(ParsedOutput("output", "output", "output", Some("1,0"), None)),
      tests.zipWithIndex.map { case (text, i) =>
        ParsedRule(s"rule$i", List(entries.getOrElseUpdate(text, unary(text))),
          List("output" -> constant(i % 2)))
      }, policy, aggregation)
  }

  // Counts input entries evaluated by the FEEL interpreter.
  private val interpretedEntries = new java.util.concurrent.atomic.AtomicInteger

  // Shared across requests, like the evaluator of a DmnEngine.
  private val evaluator = new DecisionTableEvaluator((expression, context) => {
    if (context.variables.contains(FeelEngine.UnaryTests.defaultInputVariable)) {
      interpretedEntries.incrementAndGet()
    }
    engine.literalExpressionEval.evalExpression(expression, context)
  })

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
    val result = evaluator.eval(table, context)
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
        HitPolicy.RULE_ORDER, HitPolicy.COLLECT, HitPolicy.PRIORITY,
        HitPolicy.OUTPUT_ORDER)
      value <- List[Any](1, 2, 3, 5, 8, null, "2", true)
    } {
      equivalent(table(List("1,2", "1,2", "[1..5]", "> 7", "-"), policy),
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

  it should "not retain values between requests to the same parsed table" in {
    val model = table(List("1,2", "1,2", "3", "-"), HitPolicy.RULE_ORDER)
    for (value <- List[Any](1, 3, 8, null, "1", 2, 1)) {
      equivalent(model, Map("value" -> value))
    }
  }

  it should "interpret repeated pure entries once per request" in {
    // Boolean tests are not handled by the constant fast path, so they reach
    // the interpreter; "threshold" depends on a variable and is never cached.
    val model = table(List("true", "false", "true", "false", "threshold",
      "threshold", "-"), HitPolicy.RULE_ORDER)
    for (value <- List(true, false, true)) {
      val variables = Map[String, Any]("value" -> value, "threshold" -> true)
      interpretedEntries.set(0)
      run(model, variables)
      withClue(s"value = $value") {
        interpretedEntries.get should be(4)
      }
      equivalent(model, variables)
    }
  }

  it should "preserve no-match and default-output behavior" in {
    val model = table(List("1", "1"), HitPolicy.FIRST)
    equivalent(model, Map("value" -> 9))
    equivalent(model.copy(rules = Nil), Map("value" -> 9))
    equivalent(model.copy(outputs = model.outputs.map(
      _.copy(defaultValue = Some(expression("42"))))), Map("value" -> 9))
  }

  it should "isolate simultaneous evaluations of the same parsed model" in {
    val executor = java.util.concurrent.Executors.newFixedThreadPool(4)
    implicit val executionContext: scala.concurrent.ExecutionContext =
      scala.concurrent.ExecutionContext.fromExecutorService(executor)
    try {
      val model = table(List("1,2", "1,2", "3", "-"), HitPolicy.RULE_ORDER)
      val inputs = (0 until 24).map(i => Map[String, Any]("value" -> (i % 5)))
      val expected = inputs.map(run(reference(model), _))
      val evaluated = scala.concurrent.Future.sequence(
        inputs.map(variables => scala.concurrent.Future(run(model, variables))))
      scala.concurrent.Await.result(evaluated,
        scala.concurrent.duration.Duration(10, "seconds")) should be(expected)
    } finally {
      executor.shutdownNow()
    }
  }
}
