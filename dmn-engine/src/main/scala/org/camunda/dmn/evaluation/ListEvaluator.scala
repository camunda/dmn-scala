package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{List, Expression}
import org.camunda.dmn.parser.{ParsedDecisionLogic, ParsedList}
import org.camunda.feel.syntaxtree.{Val, ValList}
import org.camunda.dmn.Audit.SingleEvaluationResult

class ListEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val]) {

  def eval(list: ParsedList, context: EvalContext): Either[Failure, Val] = {

    mapEither(list.entries, (expr: ParsedDecisionLogic) => eval(expr, context)).right
      .map(ValList)
      .map { result =>
        context.audit(list, SingleEvaluationResult(result))

        result
      }
  }

}
