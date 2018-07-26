package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser.{ ParsedDecisionLogic, ParsedRelation, ParsedRelationRow }
import org.camunda.feel.interpreter.{ Val, ValContext, DefaultContext, ValList }

class RelationEvaluator(eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val]) {

  def eval(relation: ParsedRelation, context: EvalContext): Either[Failure, Val] =
    {
      mapEither(relation.rows, (row: ParsedRelationRow) => {
        mapEither[(String, ParsedDecisionLogic), (String, Val)](row.columns, {
          case (column, expr) => eval(expr, context)
            .right
            .map(r => column -> r)
        })
          .right.map(r => ValContext(DefaultContext(r.toMap)))
      })
        .right.map(r => ValList(r))
    }

}
