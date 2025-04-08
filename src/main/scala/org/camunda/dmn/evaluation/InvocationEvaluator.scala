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

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser.{DmnRepository, EmbeddedBusinessKnowledgeModel, ImportedBusinessKnowledgeModel, ParsedBusinessKnowledgeModel, ParsedBusinessKnowledgeModelFailure, ParsedBusinessKnowledgeModelReference, ParsedExpression, ParsedInvocation}
import org.camunda.feel.syntaxtree.Val

class InvocationEvaluator(
    eval: (ParsedExpression, EvalContext) => Either[Failure, Val],
    evalBkm: (ParsedBusinessKnowledgeModel, EvalContext) => Either[Failure, Val],
    repository: DmnRepository) {

  def eval(invocation: ParsedInvocation,
           context: EvalContext): Either[Failure, Val] = {

    val result = evalParameters(invocation.bindings, context).flatMap { p =>
      val ctx = context.copy(variables = context.variables ++ p.toMap)

      resolveBkm(invocation.invocation).flatMap(evalBkm(_, ctx))
    }

    context.audit(invocation, result)
    result
  }

  private def resolveBkm(bkmRef: ParsedBusinessKnowledgeModelReference): Either[Failure, ParsedBusinessKnowledgeModel] = {
    bkmRef match {
      case ImportedBusinessKnowledgeModel(namespace, id, _) => repository.getBusinessKnowledgeModel(namespace = namespace, bkmId = id)
      case ParsedBusinessKnowledgeModelFailure(_, _, failureMessage) => Left(Failure(failureMessage))
      case bkm: EmbeddedBusinessKnowledgeModel => Right(bkm)
    }
  }

  private def evalParameters(
      bindings: Iterable[(String, ParsedExpression)],
      context: EvalContext): Either[Failure, List[(String, Any)]] = {
    mapEither[(String, ParsedExpression), (String, Any)](bindings, {
      case (name, expr) =>
        eval(expr, context)
          .map(name -> _)
    })
  }

}
