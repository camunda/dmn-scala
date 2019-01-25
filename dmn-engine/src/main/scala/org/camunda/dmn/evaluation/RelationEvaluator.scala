package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser.{
  ParsedDecisionLogic,
  ParsedRelation,
  ParsedRelationRow
}
import org.camunda.feel.interpreter.{Val, ValContext, DefaultContext, ValList}
import org.camunda.dmn.Audit.SingleEvaluationResult

class RelationEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val]) {

  def eval(relation: ParsedRelation,
           context: EvalContext): Either[Failure, Val] = {

    val rows = mapEither(
      relation.rows,
      (row: ParsedRelationRow) => {
        val columns =
          mapEither[(String, ParsedDecisionLogic), (String, Val)](row.columns, {
            case (column, expr) =>
              eval(expr, context).right
                .map(r => column -> r)
          })

        columns.right.map(values => ValContext(DefaultContext(values.toMap)))
      }
    )

    rows.right
      .map(ValList)
      .map { result =>
        context.audit(relation, SingleEvaluationResult(result))

        result
      }
  }

}
