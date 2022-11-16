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
