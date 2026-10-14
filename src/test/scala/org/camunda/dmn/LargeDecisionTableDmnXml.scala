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

object LargeDecisionTableDmnXml {

  // Builds a decision "decision" with a decision table of `ruleCount` rules
  // over two number inputs `x` and `y`. Rule i matches `x = i` (and any `y`)
  // and outputs i, so `x` selects which rule matches: 0 the first rule,
  // ruleCount - 1 the last rule, and -1 none. Mirrors the shape of the large
  // rating tables from camunda/camunda#62594.
  def decisionTableXml(ruleCount: Int, hitPolicy: String): String = {
    val rules = (0 until ruleCount)
      .map { i =>
        s"""<rule id="rule$i">
           |<inputEntry id="x$i"><text>$i</text></inputEntry>
           |<inputEntry id="y$i"><text>-</text></inputEntry>
           |<outputEntry id="out$i"><text>$i</text></outputEntry>
           |</rule>""".stripMargin
      }
      .mkString("\n")

    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
       |id="Definitions_large_table" name="Large table"
       |namespace="http://camunda.org/schema/1.0/dmn">
       |<decision id="decision" name="decision">
       |<decisionTable id="decisionTable" hitPolicy="$hitPolicy">
       |<input id="inputX"><inputExpression id="inputExpressionX" typeRef="number"><text>x</text></inputExpression></input>
       |<input id="inputY"><inputExpression id="inputExpressionY" typeRef="number"><text>y</text></inputExpression></input>
       |<output id="output" name="result" typeRef="number" />
       |$rules
       |</decisionTable>
       |</decision>
       |</definitions>""".stripMargin
  }
}
