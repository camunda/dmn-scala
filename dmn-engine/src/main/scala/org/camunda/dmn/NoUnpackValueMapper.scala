package org.camunda.dmn

import org.camunda.feel.spi.CustomValueMapper
import org.camunda.feel.interpreter.ValueMapper
import org.camunda.feel.interpreter.Val

class NoUnpackValueMapper(valueMapper: ValueMapper) extends CustomValueMapper {

  override def toVal(x: Any): Val = valueMapper.toVal(x)

  override def unpackVal(value: Val): Any = value

}
