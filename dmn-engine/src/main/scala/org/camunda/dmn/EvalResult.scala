package org.camunda.dmn

sealed trait EvalResult

object EvalNull extends EvalResult

case class EvalValue(value: Any) extends EvalResult

case class ParseFailure(error: String) extends EvalResult

case class EvalFailure(error: String) extends EvalResult