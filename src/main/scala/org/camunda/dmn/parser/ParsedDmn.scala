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
package org.camunda.dmn.parser

import org.camunda.bpm.model.dmn.DmnModelInstance

import org.camunda.bpm.model.dmn.HitPolicy
import org.camunda.bpm.model.dmn.BuiltinAggregator
import org.camunda.feel

case class ParsedDmn(model: DmnModelInstance,
                     decisions: Iterable[ParsedDecision],
                     bkms: Iterable[ParsedBusinessKnowledgeModel],
                     namespace: String) {

  val decisionsById: Map[String, ParsedDecision] =
    decisions.map(d => d.id -> d).toMap
  val decisionsByName: Map[String, ParsedDecision] =
    decisions.map(d => d.name -> d).toMap
}

// ---------------

sealed trait ParsedExpression

case class ExpressionFailure(failure: String) extends ParsedExpression

case class FeelExpression(expression: feel.syntaxtree.ParsedExpression)
    extends ParsedExpression

case object EmptyExpression extends ParsedExpression

// ---------------

sealed trait ParsedDecisionLogicContainer {
  val id: String
  val name: String
  val logic: ParsedDecisionLogic
}

case class ParsedDecision(id: String,
                          name: String,
                          logic: ParsedDecisionLogic,
                          resultName: String,
                          resultType: Option[String],
                          requiredDecisions: Iterable[ParsedDecision],
                          requiredBkms: Iterable[ParsedBusinessKnowledgeModel])
    extends ParsedDecisionLogicContainer

trait ParsedBusinessKnowledgeModel extends ParsedDecisionLogicContainer {
  val parameters: Iterable[(String, String)]
  val requiredBkms: Iterable[ParsedBusinessKnowledgeModel]
}

case class EmbeddedBusinessKnowledgeModel(
                                         id: String,
                                         name: String,
                                         logic: ParsedDecisionLogic,
                                         parameters: Iterable[(String, String)],
                                         requiredBkms: Iterable[ParsedBusinessKnowledgeModel])
  extends ParsedBusinessKnowledgeModel

case class ImportedBusinessKnowledgeModel(importer: () => ParsedBusinessKnowledgeModel) extends ParsedBusinessKnowledgeModel {
  private lazy val model = importer()
  override lazy val id: String = model.id
  override lazy val name: String = model.name
  override lazy val logic: ParsedDecisionLogic = model.logic
  override lazy val parameters: Iterable[(String, String)] = model.parameters
  override lazy val requiredBkms: Iterable[ParsedBusinessKnowledgeModel] = model.requiredBkms
}

sealed trait ParsedDecisionLogic

case class ParsedInvocation(bindings: Iterable[(String, ParsedExpression)],
                            invocation: ParsedBusinessKnowledgeModel)
    extends ParsedDecisionLogic

case class ParsedContext(entries: Iterable[(String, ParsedDecisionLogic)],
                         aggregationEntry: Option[ParsedDecisionLogic])
    extends ParsedDecisionLogic

case class ParsedList(entries: Iterable[ParsedDecisionLogic])
    extends ParsedDecisionLogic

case class ParsedRelation(rows: Iterable[ParsedRelationRow])
    extends ParsedDecisionLogic

case class ParsedRelationRow(columns: Iterable[(String, ParsedDecisionLogic)])

case class ParsedLiteralExpression(expression: ParsedExpression)
    extends ParsedDecisionLogic

case class ParsedFunctionDefinition(expression: ParsedExpression,
                                    parameters: Iterable[(String, String)])
    extends ParsedDecisionLogic

case class ParsedDecisionTable(inputs: Iterable[ParsedInput],
                               outputs: Iterable[ParsedOutput],
                               rules: Iterable[ParsedRule],
                               hitPolicy: HitPolicy,
                               aggregation: BuiltinAggregator)
    extends ParsedDecisionLogic

case class ParsedRule(id: String,
                      inputEntries: Iterable[ParsedExpression],
                      outputEntries: Iterable[(String, ParsedExpression)])

case class ParsedInput(id: String, name: String, expression: ParsedExpression)

case class ParsedOutput(id: String,
                        name: String,
                        label: String,
                        value: Option[String],
                        defaultValue: Option[ParsedExpression])
