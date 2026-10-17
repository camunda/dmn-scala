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

private[evaluation] object PureUnaryTests {
  def isPure(exp: Exp): Boolean = exp match {
    case ConstString(_) | ConstNumber(_) | ConstBool(_) | ConstNull => true
    case ConstList(items) => items.forall(isPure)
    case UnaryTestExpression(x) => isPure(x)
    case InputEqualTo(x) => isPure(x)
    case InputLessThan(x) => isPure(x)
    case InputLessOrEqual(x) => isPure(x)
    case InputGreaterThan(x) => isPure(x)
    case InputGreaterOrEqual(x) => isPure(x)
    case InputInRange(ConstRange(s, e)) => isPure(s.value) && isPure(e.value)
    case AtLeastOne(xs) => xs.forall(isPure)
    case Not(x) => isPure(x)
    case _ => false
  }
}
