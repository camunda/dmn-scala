package org.camunda.dmn

sealed trait EvalResult

case class EvalValue(key: String, value: Any) extends EvalResult

case class ParseFailure(error: String) extends EvalResult

case class EvalFailure(error: String) extends EvalResult