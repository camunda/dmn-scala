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

import java.lang.ref.WeakReference

class ParsedDmnRetentionTest extends AnyFlatSpec with Matchers {

  private val engine = new DmnEngine()
  private val engineWithLazyEvaluation =
    new DmnEngine(configuration = DmnEngine.Configuration(lazyEvaluation = true))

  "A parsed DMN" should "not keep the DMN model reachable through its required decisions" in {
    val (withoutModel, modelRef) = parseAndDropModel(engine, "/requirements/ApplicantData.dmn")

    isCollected(modelRef) should be(true)
    evaluate(engine, withoutModel, "income", Map()) should be(10000.00)
  }

  it should "not keep the DMN model reachable through its BKMs" in {
    val (withoutModel, modelRef) = parseAndDropModel(engine, "/invocation/discount.dmn")

    isCollected(modelRef) should be(true)
    evaluate(engine, withoutModel, "discount", Map("Customer" -> "Business", "OrderSize" -> 7)) should be(0.1)
  }

  it should "not keep the DMN model reachable with lazy evaluation and parse failures" in {
    val (withoutModel, modelRef) =
      parseAndDropModel(engineWithLazyEvaluation, "/config/with_invalid_bkm.dmn")

    isCollected(modelRef) should be(true)
    evaluate(engineWithLazyEvaluation, withoutModel, "greeting", Map("name" -> "DMN")) should be(
      "Hello DMN")
  }

  // Parses in a separate frame so that the caller holds no reference to the original ParsedDmn.
  private def parseAndDropModel(
      engine: DmnEngine,
      file: String): (ParsedDmn, WeakReference[AnyRef]) = {
    val parsed = engine.parse(getClass.getResourceAsStream(file)).toOption.get
    (ParsedDmn(model = null, decisions = parsed.decisions), new WeakReference(parsed.model))
  }

  private def evaluate(
      engine: DmnEngine,
      dmn: ParsedDmn,
      decisionId: String,
      variables: Map[String, Any]): Any =
    engine.eval(dmn, decisionId, variables).map(_.value).toOption.get

  // System.gc() is only a hint, so retry before concluding that the model is still reachable.
  private def isCollected(ref: WeakReference[_]): Boolean =
    (1 to 20).exists { _ =>
      System.gc()
      ref.get() == null
    }
}
