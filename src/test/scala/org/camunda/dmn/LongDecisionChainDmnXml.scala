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

object LongDecisionChainDmnXml {

  // Builds a linear, acyclic DRG: d0 -> d1 -> ... -> d(n-1), where d(n-1) = 1
  // and each d(i) = d(i+1) + 1. Mirrors the shape from camunda/dmn-scala#335.
  def chainDmnXml(decisionCount: Int): String = {
    val decisions = (0 until decisionCount)
      .map { i =>
        if (i == decisionCount - 1) {
          s"""<decision id="d$i" name="d$i">
             |<variable id="v$i" name="v$i" />
             |<literalExpression id="le$i"><text>1</text></literalExpression>
             |</decision>""".stripMargin
        } else {
          val next = i + 1
          s"""<decision id="d$i" name="d$i">
             |<variable id="v$i" name="v$i" />
             |<informationRequirement id="ir$i">
             |<requiredDecision href="#d$next" />
             |</informationRequirement>
             |<literalExpression id="le$i"><text>v$next + 1</text></literalExpression>
             |</decision>""".stripMargin
        }
      }
      .mkString("\n")

    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<definitions xmlns="https://www.omg.org/spec/DMN/20191111/MODEL/"
       |id="Definitions_chain" name="Chain"
       |namespace="http://camunda.org/schema/1.0/dmn">
       |$decisions
       |</definitions>""".stripMargin
  }
}
