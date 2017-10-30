package org.camunda.dmn

import org.camunda.bpm.model.dmn.DmnModelInstance
import org.camunda.feel.ParsedExpression

case class ParsedDmn(model: DmnModelInstance, expressionsById: Map[String, ParsedExpression])