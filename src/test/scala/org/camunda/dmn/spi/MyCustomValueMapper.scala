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

import org.camunda.feel.syntaxtree.{Val, ValString}
import org.camunda.feel.valuemapper.CustomValueMapper

class MyCustomValueMapper extends CustomValueMapper {

  override def toVal(x: Any, innerValueMapper: Any => Val): Option[Val] = {
    x match {
      case "bar" => Some(ValString("baz"))
      case _     => None
    }
  }

  override def unpackVal(value: Val,
                         innerValueMapper: Val => Any): Option[Any] = {
    value match {
      case ValString("foobar") => Some("baz")
      case _                   => None
    }
  }

}
