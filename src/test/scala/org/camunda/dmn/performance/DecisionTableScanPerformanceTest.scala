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
package org.camunda.dmn.performance

import org.camunda.bpm.model.dmn.HitPolicy
import org.camunda.dmn.evaluation.ConstantDecisionTableFixture
import org.camunda.feel.syntaxtree.ValNull
import org.scalatest.concurrent.{Signaler, ThreadSignaler, TimeLimitedTests}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import scala.util.Try

// Performance test to detect an O(n^2) regression in the decision table rule
// scan, isolated from the FEEL engine. camunda/camunda#62594
class DecisionTableScanPerformanceTest
    extends AnyFlatSpec
    with Matchers
    with TimeLimitedTests
    with ConstantDecisionTableFixture {

  // A linear rule scan completes near-instantly; a quadratic regression
  // exceeds this limit. Interrupt the test thread on timeout.
  override val timeLimit = Span(10, Seconds)
  override val defaultTestSignaler: Signaler = ThreadSignaler

  "A decision table" should "check its rules in linear time" in {
    // No rule matches, so every rule is checked, for every hit policy.
    // Collecting the result of each check by appending to an immutable List
    // is O(n^2) in the number of rules: a quadratic scan of this many rules
    // takes seconds instead of milliseconds and exceeds the time limit.
    val ruleCount = 100000
    val table =
      decisionTable(ruleCount, HitPolicy.UNIQUE, ruleMatches = _ => false)
    val expressionEval = new ConstantExpressionEval

    // Wrap the evaluation in Try so the ThreadSignaler can interrupt cleanly
    // on timeout instead of leaving the test thread hanging.
    val result = Try(evalDecisionTable(table, expressionEval))

    result.get should be(Right(ValNull))
    expressionEval.evaluatedInputEntries should be(ruleCount)
  }
}
