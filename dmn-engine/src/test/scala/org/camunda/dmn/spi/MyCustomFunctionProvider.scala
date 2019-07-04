package org.camunda.dmn.spi

import org.camunda.feel.spi.CustomFunctionProvider
import org.camunda.feel.interpreter.ValFunction
import org.camunda.feel.interpreter.ValNumber
import org.camunda.feel.interpreter.ValError

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

}
