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
package org.camunda.dmn.evaluation

import org.camunda.feel.syntaxtree._

private[evaluation] object ConstantUnaryTests {
  // None delegates to FEEL so unsupported types retain its null/error semantics.
  def evaluate(exp: Exp, input: Val): Option[Boolean] = exp match {
    case UnaryTestExpression(ConstString(s)) =>
      input match {
        case ValString(v) => Some(v == s)
        case _ => None
      }
    case UnaryTestExpression(ConstNumber(n)) => number(input)(_ == n)
    case InputLessThan(ConstNumber(n)) => number(input)(_ < n)
    case InputLessOrEqual(ConstNumber(n)) => number(input)(_ <= n)
    case InputGreaterThan(ConstNumber(n)) => number(input)(_ > n)
    case InputGreaterOrEqual(ConstNumber(n)) => number(input)(_ >= n)
    case InputInRange(ConstRange(start, end)) =>
      val lower = start match {
        case OpenConstRangeBoundary(ConstNumber(n)) => number(input)(_ > n)
        case ClosedConstRangeBoundary(ConstNumber(n)) => number(input)(_ >= n)
        case _ => None
      }
      val upper = end match {
        case OpenConstRangeBoundary(ConstNumber(n)) => number(input)(_ < n)
        case ClosedConstRangeBoundary(ConstNumber(n)) => number(input)(_ <= n)
        case _ => None
      }
      for (a <- lower; b <- upper) yield a && b
    case UnaryTestExpression(ConstList(items))
        if items.forall {
          case ConstString(_) | ConstNumber(_) => true
          case _ => false
        } =>
      input match {
        case _: ValString | _: ValNumber =>
          Some(items.exists {
            case ConstString(s) => input == ValString(s)
            case ConstNumber(n) => input == ValNumber(n)
            case _ => false
          })
        case _ => None
      }
    case AtLeastOne(tests) =>
      val results = tests.iterator.map(evaluate(_, input))
      var matched = false
      while (results.hasNext) {
        results.next() match {
          case Some(value) => matched ||= value
          case None => return None
        }
      }
      Some(matched)
    case Not(test) => evaluate(test, input).map(!_)
    case _ => None
  }

  private def number(input: Val)(predicate: BigDecimal => Boolean): Option[Boolean] =
    input match {
      case ValNumber(v) => Some(predicate(v))
      case _ => None
    }
}
