package org.camunda.dmn

import org.camunda.dmn.DmnEngine.Failure

object FunctionalHelper {
  
  def mapEither[T, R](it: Iterable[T], f: T => Either[Failure, R]): Either[Failure, List[R]] = {
    
    val emptyList: Either[Failure, List[R]] = Right(List()) 
    
    (emptyList /: it)( (xs, x) => xs.right.flatMap{xs => 
      f(x).right.map(xs :+ _)
    })  
  }
  
}