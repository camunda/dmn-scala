package org.camunda.dmn

import org.camunda.dmn.DmnEngine.Failure

object FunctionalHelper {

  def mapEither[T, R](it: Iterable[T],
                      f: T => Either[Failure, R]): Either[Failure, List[R]] = {
    foldEither[T, List[R]](List(), it, {
      case (xs, x) =>
        f(x).right.map(xs :+ _)
    })
  }

  def foldEither[T, R](start: R,
                       it: Iterable[T],
                       f: (R, T) => Either[Failure, R]): Either[Failure, R] = {

    val startValue: Either[Failure, R] = Right(start)

    (startValue /: it)((xs, x) =>
      xs.right.flatMap { xs =>
        f(xs, x)
    })
  }

}
