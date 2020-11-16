package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.feel.syntaxtree.{Val, ValFunction}
import org.camunda.bpm.model.dmn.instance.{
  Decision,
  Expression,
  BusinessKnowledgeModel,
  KnowledgeRequirement,
  InformationRequirement
}
import org.camunda.dmn.parser.{
  ParsedDecision,
  ParsedDecisionLogic,
  ParsedBusinessKnowledgeModel
}

class DecisionEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val],
    evalBkm: (ParsedBusinessKnowledgeModel,
              EvalContext) => Either[Failure, (String, ValFunction)]) {

  def eval(decision: ParsedDecision,
           context: EvalContext): Either[Failure, Val] = {

    evalDecision(decision, context)
      .map { case (name, result) => result }
  }

  private def evalDecision(
      decision: ParsedDecision,
      context: EvalContext): Either[Failure, (String, Val)] = {

    evalRequiredDecisions(decision.requiredDecisions, context)
      .flatMap(decisionResults => {
        evalRequiredKnowledge(decision.requiredBkms, context)
          .flatMap(functions => {

            val decisionEvaluationContext = context.copy(
              variables = context.variables ++ decisionResults ++ functions,
              currentElement = decision)

            eval(decision.logic, decisionEvaluationContext)
              .flatMap(
                result =>
                  decision.resultType
                    .map(typeRef => TypeChecker.isOfType(result, typeRef))
                    .getOrElse(Right(result)))

              .map(decision.resultName -> _)
          })
      })
  }

  private def evalRequiredDecisions(
      requiredDecisions: Iterable[ParsedDecision],
      context: EvalContext): Either[Failure, List[(String, Val)]] = {
    mapEither(requiredDecisions,
              (d: ParsedDecision) => evalDecision(d, context))
  }

  private def evalRequiredKnowledge(
      requiredBkms: Iterable[ParsedBusinessKnowledgeModel],
      context: EvalContext): Either[Failure, List[(String, ValFunction)]] = {
    mapEither(requiredBkms,
              (bkm: ParsedBusinessKnowledgeModel) => evalBkm(bkm, context))
  }

}
