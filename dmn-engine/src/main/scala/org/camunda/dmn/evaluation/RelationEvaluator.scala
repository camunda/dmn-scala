package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{Relation, Row, Expression}

class RelationEvaluator(eval: (Expression, EvalContext) => Either[Failure, Any]) {
  
  def eval(relation: Relation, context: EvalContext): Either[Failure, Any] = 
  {
    val columns = relation.getColumns.asScala
    val rows = relation.getRows.asScala
    
    val columNames = columns.map(_.getName)
    
    mapEither(rows, (row: Row) => 
    {
      val expressions = row.getExpressions.asScala
      
      mapEither(expressions, (expr: Expression) => eval(expr, context))
        .right
        .map(r => columNames.zip(r))
        .map(_.toMap)
    })
  }
  
}