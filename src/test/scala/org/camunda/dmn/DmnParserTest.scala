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

class DmnParserTest extends AnyFlatSpec with Matchers with DecisionTest {

  "A DMN file with cyclic dependencies between decisions" should "return an error" in {
    val failure =
      parseDmn("/requirements/cyclic-dependencies-in-decisions.dmn").failure

    failure.message should be(
      "Invalid DMN model: Cyclic dependencies between decisions detected.")
  }

  "A DMN file with non-cyclic nested dependencies" should "be parsed successfully" in {
    val parsedResult = parseDmn("/requirements/non-cyclic-nested-dependencies-in-decisions.dmn")

    parsedResult.parserResult.isRight should be(true)
  }

  "A DMN file with cyclic dependencies between BKMs" should "return an error" in {
    val failure =
      parseDmn("/requirements/cyclic-dependencies-in-bkm.dmn").failure

    failure.message should be(
      "Invalid DMN model: Cyclic dependencies between BKMs detected.")
  }


}
