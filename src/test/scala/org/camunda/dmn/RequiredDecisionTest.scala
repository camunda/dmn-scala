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
