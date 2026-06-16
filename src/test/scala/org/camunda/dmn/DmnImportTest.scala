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

import org.camunda.dmn.parser.InMemoryDmnRepository
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnImportTest extends AnyFlatSpec with Matchers with DecisionTest{

  override val engine = new DmnEngine(
    auditLogListeners = List(new TestAuditLogListener),
    dmnRepository = new InMemoryDmnRepository()
  )

  private val decisionWithBkmImport = parse("/tck/0086-import/0086-import.dmn")
  private val decisionWithDecisionImport = parse("/tck/0089-nested-inputdata-imports/0089-nested-inputdata-imports.dmn")

  // parse required DMNs
  parse("/tck/0086-import/Imported_Model.dmn")
  parse("/tck/0089-nested-inputdata-imports/Say_hello_1ID1D.dmn")
  parse("/tck/0089-nested-inputdata-imports/Model_B.dmn")
  parse("/tck/0089-nested-inputdata-imports/Model_B2.dmn")

  "A decision with an imported BKM" should "invoke the BKM from the imported DMN" in {
    eval(decisionWithBkmImport,
      "decision_with_imported_bkm",
      Map("A_Person" -> Map("name" -> "John Doe", "age" -> 21))) should be("Hello John Doe!")

    eval(decisionWithBkmImport,
      "decision_with_imported_bkm",
      Map("A_Person" -> Map("name" -> "John Doe", "age" -> 47))) should be("Respectfully, Hello John Doe!")
  }

  "A decision with a imported decisions" should "invoke the decisions from the imported DMN" in {
    val context = Map("Person_name" -> "John Doe")

    eval(decisionWithDecisionImport, "decision_with_imported_decisions", context) should be(
      "B: Evaluating Say Hello to: Hello, John Doe (B); B2: Evaluating Say Hello to: Hello, John Doe (B2)"
    )
  }

}
