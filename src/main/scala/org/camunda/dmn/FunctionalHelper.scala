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

}
