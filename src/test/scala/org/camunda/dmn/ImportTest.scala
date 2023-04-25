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
import org.camunda.dmn.model.xml.instance.{StatefulDmnModelInstanceProvider, URILocatorDmnModelInstanceProvider}
import org.camunda.dmn.parser.ParsedDmn
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.a

import scala.util.{Success, Try}

class ImportTest extends AnyFlatSpec with Matchers with DecisionTest {

  private val engineWithImportsFromURI = new DmnEngine.Builder()
    .modelInstanceProvider(new URILocatorDmnModelInstanceProvider())
    .build

  private val failResult = engineWithImportsFromURI.parse(
    getClass.getResourceAsStream("/imports/importing_model_not_found.dmn")
  )

  private val parseResult = engineWithImportsFromURI.parse(
    getClass.getResourceAsStream("/imports/importing_model.dmn")
  )

  "A model importing a dmn model" should "successfully parse" in {
    parseResult.isLeft should be(false)
  }


  it should "successfully evaluate an imported BKM" in {
    val parsedDmn = new ParsedResult(parseResult).dmn
    val result = engineWithImportsFromURI.evalByName(parsedDmn,
      "Compute Quotient",
      Map("dividend" -> 6, "divisor" -> 2))
    result.isRight should be(true)
    result.map(_.value should be(3))

  }

    it should "fail with an appropriate message when unable to import" in {
      failResult.isLeft should be(true)
      failResult.swap.toOption.map(_.message should include("Unable to load imported model"))
    }

  it should "successfully import from pre-loaded models" in {
    val engine = new DmnEngine.Builder()
      .modelInstanceProvider(new StatefulDmnModelInstanceProvider())
      .escapeNamesWithSpaces(true)
      .build

    engine.parse(
      getClass.getResourceAsStream("/imports/tck-0086-imported-model.dmn")
    ).isLeft should be(false)
    val parseRslt = engine.parse(
      getClass.getResourceAsStream("/imports/tck-0086-import.dmn")
    )
    parseRslt.isLeft should be(false)

    val parsedDmn = new ParsedResult(parseRslt).dmn

    val result = engine.evalByName(parsedDmn,
      "A Decision Ctx with DT",
      Map("A Person" -> Map("name" -> "John Doe", "age" -> 21))
    )

    result.isRight should be(true)
    result.map(_.value should be("Hello John Doe!"))

  }
}
