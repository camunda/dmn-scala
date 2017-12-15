package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.feel._
import org.camunda.feel.ParsedExpression

trait ExpressionProcessor {
  
  val feelEngine: FeelEngine
  
   def evalExpression(expression: ParsedExpression, context: Map[String, Any]): Either[Failure, Any] = {
    feelEngine.eval(expression, context) match {
      case EvalValue(value) => Right(value)
      case EvalFailure(msg) => Left(Failure(msg))
      case ParseFailure(msg) => Left(Failure(msg))
    }
  }
  
}