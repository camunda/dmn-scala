package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{ Relation, Row, Expression }
import org.camunda.dmn.parser.ParsedDecisionLogic
import org.camunda.dmn.parser.ParsedRelation
import org.camunda.dmn.parser.ParsedRelationRow
import org.camunda.dmn.parser.ParsedDecisionLogic
import org.camunda.dmn.parser.ParsedDecisionLogic

class RelationEvaluator(eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Any]) {

  def eval(relation: ParsedRelation, context: EvalContext): Either[Failure, Any] =
    {
      mapEither(relation.rows, (row: ParsedRelationRow) => {
        mapEither[(String, ParsedDecisionLogic), (String, Any)](row.columns, {
          case (column, expr) => eval(expr, context)
            .right
            .map(r => column -> r)
        })
          .right.map(_.toMap)
      })
    }

}
