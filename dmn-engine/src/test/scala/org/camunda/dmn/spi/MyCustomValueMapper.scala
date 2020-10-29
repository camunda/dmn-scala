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
