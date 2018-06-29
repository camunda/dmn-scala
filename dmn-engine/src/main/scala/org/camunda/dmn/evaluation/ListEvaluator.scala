package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{ List, Expression }
import org.camunda.dmn.parser.ParsedDecisionLogic
import org.camunda.dmn.parser.ParsedList
import org.camunda.dmn.parser.ParsedDecisionLogic

class ListEvaluator(eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Any]) {

  def eval(list: ParsedList, context: EvalContext): Either[Failure, Any] = {
    mapEither(list.entries, (expr: ParsedDecisionLogic) => eval(expr, context))
  }

}
