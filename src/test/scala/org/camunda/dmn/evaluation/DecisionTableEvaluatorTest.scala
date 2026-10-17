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
import org.camunda.feel.syntaxtree.ValNumber
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// Unit coverage for camunda/camunda#62594, isolated from the FEEL engine.
class DecisionTableEvaluatorTest
    extends AnyFlatSpec
    with Matchers
    with ConstantDecisionTableFixture {

  "A decision table with hit policy FIRST" should "stop checking rules after the first matching rule" in {
    val table =
      decisionTable(ruleCount = 1000, HitPolicy.FIRST, ruleMatches = _ => true)
    val expressionEval = new ConstantExpressionEval

    evalDecisionTable(table, expressionEval) should be(Right(ValNumber(0)))
    expressionEval.evaluatedInputEntries should be(1)
  }

  "A decision table with any-input entries" should "not evaluate them" in {
    val table = decisionTableWithEntries(
      ruleCount = 3,
      HitPolicy.FIRST,
      inputEntries = i => List(anyInput, constant(i == 2), anyInput))
    val expressionEval = new ConstantExpressionEval

    evalDecisionTable(table, expressionEval) should be(Right(ValNumber(2)))
    expressionEval.evaluatedInputEntries should be(3)
  }
}
