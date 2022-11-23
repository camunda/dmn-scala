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

import org.camunda.feel.syntaxtree.Val
import org.camunda.feel.valuemapper.{CustomValueMapper, ValueMapper}

class NoUnpackValueMapper(valueMapper: ValueMapper) extends CustomValueMapper {

  override def toVal(x: Any, innerValueMapper: Any => Val): Option[Val] =
    Some(valueMapper.toVal(x))

  override def unpackVal(value: Val,
                         innerValueMapper: Val => Any): Option[Any] =
    Some(value)

}
