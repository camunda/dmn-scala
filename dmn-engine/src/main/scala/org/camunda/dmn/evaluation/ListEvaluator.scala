package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{List, Expression}

class ListEvaluator(eval: (Expression, EvalContext) => Either[Failure, Any]) {
  
  def eval(list: List, context: EvalContext): Either[Failure, Any] = 
  {
    val expressions = list.getExpressions.asScala
    
    mapEither(expressions, (expr: Expression) => eval(expr, context))
  }
  
}