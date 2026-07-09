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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

class LongDecisionChainEndToEndTest extends AnyFlatSpec with Matchers with DecisionTest {

  // Same shape as the issue's reported repro (decision1 -> decision2 -> ...
  // -> decisionN): deploy (parse), then evaluate, exactly as reported. Kept
  // at the same, smaller count as LongDecisionChainParsingTest (not the
  // evaluator's 10,000) since parsing here goes through the same O(n^2)
  // cycle-detection walk on an acyclic chain — see Task 4's note.
  private val decisionCount = 3000

  "A deployed long decision chain" should "parse and evaluate successfully end to end" in {
    val xml = LongDecisionChainDmnXml.chainDmnXml(decisionCount)
    val stream = new ByteArrayInputStream(xml.getBytes(StandardCharsets.UTF_8))
    val chain = new ParsedResult(engine.parse(stream)).dmn

    eval(chain, "d0", Map()) should be(decisionCount)
  }
}
