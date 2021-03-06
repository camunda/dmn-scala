package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser.{
  ParsedDecisionLogic,
  ParsedRelation,
  ParsedRelationRow
}
import org.camunda.feel.context.Context.StaticContext
import org.camunda.feel.syntaxtree.{Val, ValContext, ValError, ValList}

class RelationEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val]) {

  def eval(relation: ParsedRelation,
           context: EvalContext): Either[Failure, Val] = {

    val result = mapEither(
      relation.rows,
      (row: ParsedRelationRow) => {
        val columns =
          mapEither[(String, ParsedDecisionLogic), (String, Val)](row.columns, {
            case (column, expr) =>
              eval(expr, context)
                .map(r => column -> r)
          })

        columns.map(values => ValContext(StaticContext(values.toMap)))
      }
    ).map(ValList)

    context.audit(relation, result)
    result
  }

}
