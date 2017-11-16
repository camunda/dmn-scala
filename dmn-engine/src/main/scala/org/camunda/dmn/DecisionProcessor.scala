package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.feel._
import org.camunda.feel.ParsedExpression

trait DecisionProcessor {
  
  val feelEngine: FeelEngine
  
   def evalExpression(expression: ParsedExpression, context: Map[String, Any]): Either[Failure, Any] = {
    feelEngine.eval(expression, context) match {
      case EvalValue(value) => Right(value)
      case EvalFailure(msg) => Left(Failure(msg))
      case ParseFailure(msg) => Left(Failure(msg))
    }
  }
  
  def mapEither[T, R](it: Iterable[T], f: T => Either[Failure, R]): Either[Failure, List[R]] = {
    
    val emptyList: Either[Failure, List[R]] = Right(List()) 
    
    (emptyList /: it)( (xs, x) => xs.right.flatMap{xs => 
      f(x).right.map(xs :+ _)
    })  
  }
  
}