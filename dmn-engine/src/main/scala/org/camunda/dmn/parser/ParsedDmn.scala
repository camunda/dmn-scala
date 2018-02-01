package org.camunda.dmn.parser

import org.camunda.bpm.model.dmn.DmnModelInstance
import org.camunda.feel.ParsedExpression
import scala.collection.JavaConverters._
import org.camunda.bpm.model.dmn.instance.Decision

case class ParsedDmn(
  model: DmnModelInstance, 
  expressions: Map[String, ParsedExpression],
  unaryTests: Map[String, ParsedExpression]) {
  
  val decisions: List[Decision] = model.getDefinitions
    .getDrgElements
    .asScala
    .filter(_.isInstanceOf[Decision])
    .map(_.asInstanceOf[Decision])
    .toList
  
}