package org.camunda.dmn.parser

import org.camunda.bpm.model.dmn.DmnModelInstance
import org.camunda.feel.ParsedExpression

case class ParsedDmn(
  model: DmnModelInstance, 
  expressions: Map[String, ParsedExpression],
  unaryTests: Map[String, ParsedExpression])