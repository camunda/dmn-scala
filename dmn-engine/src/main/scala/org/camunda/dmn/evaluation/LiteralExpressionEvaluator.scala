package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.feel._
import org.camunda.feel.interpreter.{RootContext, ValFunction}
import org.camunda.bpm.model.dmn.instance.{LiteralExpression, UnaryTests}
import scala.Left
import scala.Right
import org.camunda.feel.interpreter.DefaultContext

class LiteralExpressionEvaluator(feelEngine: FeelEngine) {
  
  def evalUnaryTests(unaryTests: UnaryTests, context: EvalContext): Either[Failure, Any] = 
  {
    val text = unaryTests.getText.getTextContent
    val expr = context.parsedUnaryTests.get(text)
    
    expr
      .map(eval(_, context))
      .getOrElse(Left(Failure(s"no parsed unary-tests found for '$text'")))
  }
  
  def evalExpression(literalExpression: LiteralExpression, context: EvalContext): Either[Failure, Any] = 
  {
    val text = literalExpression.getText.getTextContent
    val expr = context.parsedExpressions.get(text)
    
    expr
      .map(eval(_, context))
      .getOrElse(Left(Failure(s"no parsed expression found for '$text'")))
  }
  
  private def eval(expression: ParsedExpression, context: EvalContext): Either[Failure, Any] = 
  {
    val functions = context.variables
      .filter{ case (k,v) => v.isInstanceOf[ValFunction]}
      .map{ case (k,f) => k -> List(f.asInstanceOf[ValFunction])}
    
    val evalContext = DefaultContext(
      variables = context.variables,
      functions = functions)
    
    feelEngine.eval(expression, evalContext) match {
        case EvalValue(value) => Right(value)
        case EvalFailure(msg) => Left(Failure(msg))
        case ParseFailure(msg) => Left(Failure(msg))
    }
  }
  
}