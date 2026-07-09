/*
 * Copyright © 2022 Camunda Services GmbH (info@camunda.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.camunda.dmn

import org.camunda.dmn.DmnEngine.Failure

import scala.util.control.TailCalls._

object FunctionalHelper {

  def mapEither[T, R](it: Iterable[T],
                      f: T => Either[Failure, R]): Either[Failure, List[R]] = {
    foldEither[T, List[R]](List(), it, {
      case (xs, x) =>
        f(x).map(xs :+ _)
    })
  }

  def foldEither[T, R](start: R,
                       it: Iterable[T],
                       f: (R, T) => Either[Failure, R]): Either[Failure, R] = {

    val startValue: Either[Failure, R] = Right(start)

    (startValue /: it)((xs, x) =>
      xs.flatMap { xs =>
        f(xs, x)
    })
  }

  // TailRec-safe variants used where `f` may itself recurse arbitrarily deep
  // (e.g. evaluating a chain of dependent decisions). These preserve the same
  // left-to-right, short-circuit-on-first-failure semantics as mapEither/
  // foldEither above, but never grow the JVM call stack.

  def mapEitherTailRec[T, R](
      it: Iterable[T],
      f: T => TailRec[Either[Failure, R]]): TailRec[Either[Failure, List[R]]] = {
    // Accumulate by prepending (O(1) per element) and reverse once at the
    // end, rather than appending with `:+` (O(n) per element, O(n^2) total)
    // — this helper is meant to support large traversals, so it should stay
    // linear in `it`.
    foldEitherTailRec[T, List[R]](List(), it, {
      case (xs, x) =>
        tailcall(f(x)).map(_.map(_ :: xs))
    }).map(_.map(_.reverse))
  }

  def foldEitherTailRec[T, R](
      start: R,
      it: Iterable[T],
      f: (R, T) => TailRec[Either[Failure, R]]): TailRec[Either[Failure, R]] = {

    val iterator = it.iterator

    def loop(acc: Either[Failure, R]): TailRec[Either[Failure, R]] = {
      acc match {
        case Left(_) => done(acc)
        case Right(value) =>
          if (!iterator.hasNext) {
            done(acc)
          } else {
            tailcall(f(value, iterator.next())).flatMap(loop)
          }
      }
    }

    loop(Right(start))
  }

}
