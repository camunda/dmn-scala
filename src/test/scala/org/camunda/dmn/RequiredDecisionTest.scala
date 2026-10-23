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

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.ParsedDmn
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.lang.ref.WeakReference

class RequiredDecisionTest extends AnyFlatSpec with Matchers with DecisionTest {

  lazy val discountDecision = parse("/requirements/discount.dmn")
  lazy val applicantDataDecision = parse("/requirements/ApplicantData.dmn")

  "A decision" should "evaluate a required decision" in {
    eval(discountDecision,
         "price",
         Map("customer" -> "Business", "orderSize" -> 7)) should be(10)
  }

  it should "evaluate multiple required decision" in {
    eval(applicantDataDecision, "income", Map()) should be(10000.00)
  }

  it should "not keep the DMN model reachable through its required decisions" in {
    val (withoutModel, modelRef) = parseAndDropModel("/requirements/ApplicantData.dmn")

    isCollected(modelRef) should be(true)
    eval(withoutModel, "income", Map()) should be(10000.00)
  }

  // camunda/dmn-scala#394: a required decision that is reachable over
  // multiple paths is evaluated only once per evaluation.
  "A required decision" should "be evaluated once if it is required once" in {
    eval(sharedRequiredDecisions, "Single", Map("x" -> 1)) should be(2)

    evaluatedDecisions should be(List("D", "Single"))
  }

  it should "be evaluated once if it is required by sibling decisions" in {
    eval(sharedRequiredDecisions, "Diamond", Map("x" -> 1)) should be(10)

    evaluatedDecisions should be(List("D", "B", "C", "Diamond"))
  }

  it should "be evaluated once if it is also required directly" in {
    eval(sharedRequiredDecisions, "DiamondDirect", Map("x" -> 1)) should be(12)

    evaluatedDecisions should be(List("D", "B", "C", "DiamondDirect"))
  }

  it should "be evaluated once if it is reachable over many paths" in {
    eval(sharedRequiredDecisions, "Top", Map("x" -> 1)) should be(298)

    evaluatedDecisions should be(List("D", "Y1", "Y2", "X1", "X2", "Top"))
  }

  it should "not reuse results across evaluations" in {
    eval(sharedRequiredDecisions, "Diamond", Map("x" -> 1)) should be(10)
    eval(sharedRequiredDecisions, "Diamond", Map("x" -> 2)) should be(15)

    evaluatedDecisions should be(List("D", "B", "C", "Diamond"))
  }

  it should "fail the evaluation if it fails, without evaluating it again" in {
    eval(sharedRequiredDecisions, "FailingDiamond", Map("x" -> -1)) shouldBe a[
      Failure]

    evaluatedDecisions should be(List("F"))
  }

  it should "be evaluated once if its result is null" in {
    eval(sharedRequiredDecisions, "NullDiamond", Map("x" -> 1)) should equal(
      true)

    evaluatedDecisions should be(List("N", "NB", "NC", "NullDiamond"))
  }

  // The reuse relies on this: a required decision only sees the input
  // variables, not the results of the decisions evaluated before it.
  it should "not see the results of sibling decisions" in {
    // ShadowX's result is named "x", like the input variable that D reads
    eval(sharedRequiredDecisions, "Isolated", Map("x" -> 1)) should be(6)

    evaluatedDecisions should be(List("ShadowX", "D", "C", "Isolated"))
  }

  private lazy val sharedRequiredDecisions =
    parse("/requirements/shared-required-decisions.dmn")

  private def evaluatedDecisions: List[String] = auditLog.entries.map(_.id)

  private def parseAndDropModel(file: String): (ParsedDmn, WeakReference[AnyRef]) = {
    val parsed = parse(file)
    (ParsedDmn(model = null, decisions = parsed.decisions), new WeakReference(parsed.model))
  }

  private def isCollected(ref: WeakReference[_]): Boolean =
    (1 to 20).exists { _ =>
      System.gc()
      ref.get() == null
    }
}
