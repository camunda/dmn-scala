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

import org.camunda.dmn.DecisionTest
import org.camunda.dmn.DmnEngine.{EvalContext, Failure}
import org.camunda.dmn.StackSizeTestSupport
import org.camunda.dmn.parser.{EmptyExpression, ParsedDecision, ParsedDmn, ParsedLiteralExpression}
import org.camunda.feel.syntaxtree.ValNumber
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LongDecisionChainEvaluatorTest
    extends AnyFlatSpec
    with Matchers
    with DecisionTest
    with StackSizeTestSupport {

  private val decisionCount = 10000

  // Reuses an existing, already-parsed test fixture purely to obtain a real
  // DmnModelInstance to satisfy ParsedDmn's constructor. Its content is
  // irrelevant here: the stub `eval` below never reads decision.logic
  // through the real FEEL engine, so this test is isolated to
  // DecisionEvaluator's own recursion and unaffected by the parser (which
  // has its own, separately-tested and separately-fixed recursion bug — see
  // LongDecisionChainParsingTest).
  private val dummyModel = parse("/requirements/discount.dmn").model

  // Builds a linear chain of ParsedDecision objects, d0 -> d1 -> ... -> d(n-1),
  // constructed iteratively from the tail (foldRight over a Range is
  // implemented iteratively in the Scala standard library) so building the
  // fixture itself can never be the thing that overflows.
  private def buildChain(decisionCount: Int): ParsedDecision = {
    val leaf = ParsedDecision(
      id = s"d${decisionCount - 1}",
      name = s"d${decisionCount - 1}",
      logic = ParsedLiteralExpression(EmptyExpression),
      resultName = s"v${decisionCount - 1}",
      resultType = None,
      requiredDecisions = Nil,
      requiredBkms = Nil
    )

    (0 until decisionCount - 1).foldRight(leaf) { (i, next) =>
      ParsedDecision(
        id = s"d$i",
        name = s"d$i",
        logic = ParsedLiteralExpression(EmptyExpression),
        resultName = s"v$i",
        resultType = None,
        requiredDecisions = List(next),
        requiredBkms = Nil
      )
    }
  }

  private def newEvaluator(): DecisionEvaluator =
    new DecisionEvaluator(
      eval = (_, _) => Right(ValNumber(1)),
      evalBkm = (_, _) => Left(Failure("not required by this test"))
    )

  "A decision evaluator" should "evaluate a long chain of required decisions without a StackOverflowError" in {
    val root = buildChain(decisionCount)
    val evaluator = newEvaluator()
    val context = EvalContext(
      dmn = ParsedDmn(dummyModel, root :: Nil),
      variables = Map.empty,
      currentElement = root
    )

    val result = runWithStackSize(smallStackSize) {
      evaluator.eval(root, context)
    }

    result should be(Right(ValNumber(1)))
  }
}
