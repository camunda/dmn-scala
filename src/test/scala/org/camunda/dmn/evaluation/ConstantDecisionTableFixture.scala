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
package org.camunda.dmn.evaluation

import org.camunda.bpm.model.dmn.HitPolicy
import org.camunda.dmn.DecisionTest
import org.camunda.dmn.DmnEngine.{EvalContext, Failure}
import org.camunda.dmn.parser.{
  FeelExpression,
  ParsedDecision,
  ParsedDecisionTable,
  ParsedDmn,
  ParsedExpression,
  ParsedInput,
  ParsedOutput,
  ParsedRule
}
import org.camunda.feel.syntaxtree.{ConstBool, ConstNumber, Val, ValBoolean, ValNumber}

// Shared fixtures for camunda/camunda#62594, isolated from the FEEL engine:
// every expression of the decision tables below is a constant, evaluated by
// ConstantExpressionEval instead of the real FEEL engine.
trait ConstantDecisionTableFixture extends DecisionTest {

  // Reuses an existing, already-parsed test fixture purely to obtain a real
  // DmnModelInstance to satisfy ParsedDmn's constructor. Its content is
  // irrelevant here.
  protected val dummyModel = parse("/decisiontable/discount.dmn").model

  protected def constant(value: Boolean): ParsedExpression =
    FeelExpression(
      org.camunda.feel.syntaxtree.ParsedExpression(ConstBool(value),
                                                   value.toString))

  protected def constant(value: Int): ParsedExpression =
    FeelExpression(
      org.camunda.feel.syntaxtree.ParsedExpression(ConstNumber(value),
                                                   value.toString))

  // A decision table with one input and one output. Rule i matches if
  // `ruleMatches(i)` and outputs i.
  protected def decisionTable(
      ruleCount: Int,
      hitPolicy: HitPolicy,
      ruleMatches: Int => Boolean): ParsedDecisionTable =
    ParsedDecisionTable(
      inputs = List(ParsedInput("input", "input", constant(1))),
      outputs = List(ParsedOutput("output", "output", "output", None, None)),
      rules = (0 until ruleCount).map { i =>
        ParsedRule(id = s"rule$i",
                   inputEntries = List(constant(ruleMatches(i))),
                   outputEntries = List("output" -> constant(i)))
      }.toList,
      hitPolicy = hitPolicy,
      aggregation = null
    )

  // Evaluates the constant expressions of `decisionTable`, and counts the
  // evaluated input entries (its only boolean expressions).
  protected class ConstantExpressionEval
      extends ((ParsedExpression, EvalContext) => Either[Failure, Val]) {

    var evaluatedInputEntries = 0

    override def apply(expression: ParsedExpression,
                       context: EvalContext): Either[Failure, Val] =
      expression match {
        case FeelExpression(parsed) =>
          parsed.expression match {
            case ConstBool(value) =>
              evaluatedInputEntries += 1
              Right(ValBoolean(value))
            case ConstNumber(value) => Right(ValNumber(value))
            case other => Left(Failure(s"unexpected expression '$other'"))
          }
        case other => Left(Failure(s"unexpected expression '$other'"))
      }
  }

  protected def evalDecisionTable(
      table: ParsedDecisionTable,
      expressionEval: ConstantExpressionEval): Either[Failure, Val] = {
    val decision = ParsedDecision(
      id = "decision",
      name = "decision",
      logic = table,
      resultName = "output",
      resultType = None,
      requiredDecisions = Nil,
      requiredBkms = Nil
    )
    val context = EvalContext(
      dmn = ParsedDmn(dummyModel, decision :: Nil),
      variables = Map.empty,
      currentElement = decision
    )

    new DecisionTableEvaluator(expressionEval).eval(table, context)
  }
}
