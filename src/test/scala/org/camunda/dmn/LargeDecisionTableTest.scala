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
package org.camunda.dmn

import org.camunda.dmn.parser.ParsedDmn
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

// End-to-end coverage for camunda/camunda#62594: large decision tables,
// parsed from DMN XML and evaluated through the engine's public API.
class LargeDecisionTableTest extends AnyFlatSpec with Matchers with DecisionTest {

  private def parseDecisionTable(ruleCount: Int, hitPolicy: String): ParsedDmn = {
    val xml = LargeDecisionTableDmnXml.decisionTableXml(ruleCount, hitPolicy)
    val stream = new ByteArrayInputStream(xml.getBytes(StandardCharsets.UTF_8))
    new ParsedResult(engine.parse(stream)).dmn
  }

  private def evalWithX(dmn: ParsedDmn, x: Int): Any =
    eval(dmn, "decision", Map("x" -> x, "y" -> 1))

  // The smallest wall-clock time of several runs, after a warm-up, to filter
  // out noise from JIT compilation and garbage collection.
  private def minDurationNanos(body: => Any): Long = {
    (1 to 3).foreach(_ => body)
    (1 to 5).map { _ =>
      val start = System.nanoTime()
      body
      System.nanoTime() - start
    }.min
  }

  "A decision table with hit policy FIRST" should "return the first match even if a later rule fails to evaluate" in {
    val dmn = parse("/decisiontable/first_with_failing_rule_after_match.dmn")

    eval(dmn, "decision", Map("amount" -> 10)) should be("first match")
  }

  it should "stop checking rules once the first rule matches" in {
    val ruleCount = 10000
    val dmn = parseDecisionTable(ruleCount, "FIRST")

    evalWithX(dmn, 0) should be(0)
    evalWithX(dmn, ruleCount - 1) should be(ruleCount - 1)

    // Matching the first rule only needs to check one rule; matching the
    // last one needs to check all of them.
    val firstRuleMatches = minDurationNanos(evalWithX(dmn, 0))
    val lastRuleMatches = minDurationNanos(evalWithX(dmn, ruleCount - 1))

    firstRuleMatches should be < lastRuleMatches / 4
  }

  "A decision table with hit policy UNIQUE" should "check its rules in linear time" in {
    val dmn = parseDecisionTable(ruleCount = 40000, "UNIQUE")

    evalWithX(dmn, -1) should be(None)

    // No rule matches, so every rule is checked. A linear scan of 40,000
    // rules takes milliseconds, a quadratic one seconds.
    val duration = minDurationNanos(evalWithX(dmn, -1))

    TimeUnit.NANOSECONDS.toMillis(duration) should be < 500L
  }
}
