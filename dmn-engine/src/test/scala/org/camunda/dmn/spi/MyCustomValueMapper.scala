package org.camunda.dmn.spi

import org.camunda.feel._
import org.camunda.feel.interpreter._
import org.camunda.feel.spi.CustomValueMapper

class MyCustomValueMapper extends CustomValueMapper {

  override def toVal(x: Any): Val =
    {
      x match {
        case "bar" => ValString("baz")
        case _ => super.toVal(x)
      }
    }

  override def unpackVal(value: Val): Any =
    {
      value match {
        case ValString("foobar") => "baz"
        case _ => super.unpackVal(value)
      }
    }

}