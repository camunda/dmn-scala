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

class LongDecisionChainParsingTest
    extends AnyFlatSpec
    with Matchers
    with DecisionTest
    with StackSizeTestSupport {

  // Deliberately much smaller than the evaluator test's 10,000: the parser's
  // cycle-detection walk (DmnParser.hasDependencyCycle) is O(n^2) on an
  // acyclic chain once trampolined, since `.exists` runs a full DFS from
  // every start node when none of them find a cycle. This only needs to be
  // large enough to reliably overflow a small stack pre-fix, not to
  // stress-test throughput.
  private val decisionCount = 3000

  "The DMN parser" should "parse a long chain of acyclic decisions without a StackOverflowError" in {
    val xml = LongDecisionChainDmnXml.chainDmnXml(decisionCount)

    val parseResult = runWithStackSize(smallStackSize) {
      val stream = new ByteArrayInputStream(xml.getBytes(StandardCharsets.UTF_8))
      engine.parse(stream)
    }

    parseResult.isRight should be(true)
  }
}
