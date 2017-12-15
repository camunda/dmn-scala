package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.feel._
import org.camunda.feel.ParsedExpression
import org.camunda.bpm.model.dmn.instance.{LiteralExpression, UnaryTests}

class LiteralExpressionProcessor(feelEngine: FeelEngine) {
  
  def evalUnaryTests(unaryTests: UnaryTests, context: EvalContext): Either[Failure, Any] = 
    eval(unaryTests.getText.getTextContent, context)
  
  def eval(literalExpression: LiteralExpression, context: EvalContext): Either[Failure, Any] = 
    eval(literalExpression.getText.getTextContent, context) 
  
  private def eval(expr: String, context: EvalContext): Either[Failure, Any] = {
    
    val expression = context.parsedExpressions(expr)
    
    feelEngine.eval(expression, context.variables) match {
        case EvalValue(value) => Right(value)
        case EvalFailure(msg) => Left(Failure(msg))
        case ParseFailure(msg) => Left(Failure(msg))
    }
  }
  
}