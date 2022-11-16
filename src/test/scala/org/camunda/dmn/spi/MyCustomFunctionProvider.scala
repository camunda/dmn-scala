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
