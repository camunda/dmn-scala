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
package org.camunda.dmn.spi

import org.camunda.feel.context.CustomFunctionProvider
import org.camunda.feel.syntaxtree.{ValError, ValFunction, ValNumber}

class MyCustomFunctionProvider extends CustomFunctionProvider {

  val functions = Map(
    "incr" ->
      ValFunction(
        params = List("x"),
        invoke = (args) =>
          args.head match {
            case ValNumber(x) => ValNumber(x + 1)
            case x            => ValError(s"expected number but found '$x'")
        }
      )
  )

  def getFunction(name: String): Option[ValFunction] = functions.get(name)

  override def functionNames(): Iterable[String] = functions.keys
}
