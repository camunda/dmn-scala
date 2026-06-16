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
import org.camunda.dmn.parser.{DmnRepository, EmbeddedBusinessKnowledgeModel, ExpressionFailure, ImportedBusinessKnowledgeModel, ParsedBusinessKnowledgeModel, ParsedBusinessKnowledgeModelFailure, ParsedBusinessKnowledgeModelReference, ParsedDecisionLogic}
import org.camunda.feel.syntaxtree.{Val, ValError, ValFunction}
import org.camunda.feel.valuemapper.ValueMapper

class BusinessKnowledgeEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val],
    valueMapper: ValueMapper,
    repository: DmnRepository) {

  def eval(bkm: ParsedBusinessKnowledgeModel,
           context: EvalContext): Either[Failure, Val] = {

    resolveRequiredBkms(bkm)
      .flatMap(evalRequiredKnowledge(_, context))
      .flatMap(functions => {

        val evalContext =
          context.copy(variables = context.variables ++ functions,
                       currentElement = bkm)

        validateParameters(bkm.parameters, evalContext)
          .flatMap(_ => eval(bkm.logic, evalContext))
      })
  }

  private def resolveRequiredBkms(bkm: ParsedBusinessKnowledgeModel): Either[Failure, Iterable[ParsedBusinessKnowledgeModel]] = {
    mapEither[ParsedBusinessKnowledgeModelReference, ParsedBusinessKnowledgeModel](bkm.requiredBkms, {
      case ImportedBusinessKnowledgeModel(namespace, id, _) => repository.getBusinessKnowledgeModel(namespace = namespace, bkmId = id)
      case ParsedBusinessKnowledgeModelFailure(_, _, failureMessage) => Left(Failure(failureMessage))
      case bkm: EmbeddedBusinessKnowledgeModel => Right(bkm)
    })
  }

  def createFunction(
      bkm: ParsedBusinessKnowledgeModel,
      context: EvalContext): Either[Failure, (String, ValFunction)] = {

    resolveRequiredBkms(bkm)
      .flatMap(evalRequiredKnowledge(_, context))
      .map(functions => {

      val evalContext = context.copy(variables = context.variables ++ functions,
                                     currentElement = bkm)

      val function = createFunction(bkm.logic, bkm.parameters, evalContext)

      bkm.name -> function
    })
  }

  private def evalRequiredKnowledge(
      knowledgeRequirements: Iterable[ParsedBusinessKnowledgeModel],
      context: EvalContext): Either[Failure, List[(String, ValFunction)]] = {
    mapEither(
      knowledgeRequirements,
      (bkm: ParsedBusinessKnowledgeModel) => createFunction(bkm, context))
  }

  private def validateArguments(
      parameters: Iterable[(String, String)],
      args: List[Val],
      context: EvalContext): Either[Failure, List[(String, Val)]] = {
    mapEither[((String, String), Val), (String, Val)](parameters.zip(args), {
      case ((name, typeRef), arg) =>
        TypeChecker
          .isOfType(arg, typeRef)
          .map(name -> _)
    })
  }

  private def validateParameters(
      parameters: Iterable[(String, String)],
      context: EvalContext): Either[Failure, List[Any]] = {
    mapEither[(String, String), Any](
      parameters, {
        case (name, typeRef) =>
          context.variables
            .get(name)
            .map(v => TypeChecker.isOfType(valueMapper.toVal(v), typeRef))
            .getOrElse(Left(Failure(s"no parameter found with name '${name}'")))
      }
    )
  }

  private def createFunction(expression: ParsedDecisionLogic,
                             parameters: Iterable[(String, String)],
                             context: EvalContext): ValFunction = {
    val parameterNames = parameters.map(_._1).toList

    ValFunction(
      params = parameterNames,
      invoke = args => {

        val result = validateArguments(parameters, args, context).flatMap(
          arguments =>
            eval(expression,
                 context.copy(variables = context.variables ++ arguments)))

        result match {
          case Right(value)  => value
          case Left(failure) => ValError(failure.message)
        }
      }
    )
  }

}
