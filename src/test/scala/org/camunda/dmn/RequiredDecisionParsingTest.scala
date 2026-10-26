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

class RequiredDecisionParsingTest extends AnyFlatSpec with Matchers with DecisionTest {

  private def requiredIds(dmn: ParsedDmn, id: String): List[String] =
    dmn.decisionsById(id).requiredDecisions.map(_.id).toList

  "A parsed decision" should "reference all required decisions in declaration order" in {
    val dmn = parse("/requirements/ApplicantData.dmn")

    requiredIds(dmn, "income") should be(List("applicantData", "monthly"))
    requiredIds(dmn, "applicantData") shouldBe empty
  }

  it should "reference required decisions that are defined later in the file" in {
    val dmn = parse("/requirements/non-cyclic-nested-dependencies-in-decisions.dmn")

    requiredIds(dmn, "C") should be(List("A", "B"))
    requiredIds(dmn, "B") should be(List("A"))
    requiredIds(dmn, "A") shouldBe empty
  }

  it should "share one parsed instance of a decision required by several decisions" in {
    val dmn = parse("/requirements/non-cyclic-nested-dependencies-in-decisions.dmn")
    val a = dmn.decisionsById("A")

    dmn.decisions.map(_.id).toList.sorted should be(List("A", "B", "C"))
    dmn.decisionsById("C").requiredDecisions.head should be theSameInstanceAs a
    dmn.decisionsById("B").requiredDecisions.head should be theSameInstanceAs a
  }

  "A decision with diamond-shaped requirements" should "be evaluated" in {
    val dmn = parse("/requirements/non-cyclic-nested-dependencies-in-decisions.dmn")

    eval(dmn, "C", Map()) should be(List(1, 2))
  }

  "Two decisions requiring the same decision" should "both be evaluated" in {
    val dmn = parse("/requirements/shared-required-decision-of-two-roots.dmn")

    requiredIds(dmn, "first") should be(List("shared"))
    requiredIds(dmn, "second") should be(List("shared"))
    eval(dmn, "first", Map("base" -> 1, "offset" -> 5)) should be(7)
    eval(dmn, "second", Map("base" -> 1)) should be(20)
  }

  "A decision requiring the same decision twice" should "be evaluated" in {
    val dmn = parse("/requirements/duplicate-required-decision.dmn")

    requiredIds(dmn, "root") should be(List("shared", "shared"))
    eval(dmn, "root", Map()) should be(2)
  }

  "A decision requiring itself" should "be rejected as a cyclic dependency" in {
    parseDmn("/requirements/self-dependency-in-decision.dmn").failure.message should be(
      "Invalid DMN model: Cyclic dependencies between decisions detected.")
  }

  "A decision requiring an invalid decision" should "fail to parse" in {
    parseDmn("/requirements/invalid-required-decision.dmn").parserResult.isLeft should be(true)
  }

  it should "fail on evaluation with lazy evaluation" in {
    val lazyEngine =
      new DmnEngine(configuration = DmnEngine.Configuration(lazyEvaluation = true))
    val dmn = lazyEngine
      .parse(getClass.getResourceAsStream("/requirements/invalid-required-decision.dmn"))
      .toOption
      .get

    requiredIds(dmn, "root") should be(List("broken"))
    lazyEngine.eval(dmn, "root", Map[String, Any]()).isLeft should be(true)
  }
}
