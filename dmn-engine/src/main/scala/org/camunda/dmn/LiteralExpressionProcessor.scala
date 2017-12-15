package org.camunda.dmn

import org.camunda.bpm.model.dmn.instance.LiteralExpression
import org.camunda.dmn.DmnEngine._
import org.camunda.feel.FeelEngine

class LiteralExpressionProcessor(val feelEngine: FeelEngine) extends ExpressionProcessor {
  
  def eval(literalExpression: LiteralExpression)(implicit context: EvalContext): Either[Failure, Any] = 
  {
    val expression = context.parsedExpressions(literalExpression.getId)
    
    evalExpression(expression, context.variables)
  }
  
}