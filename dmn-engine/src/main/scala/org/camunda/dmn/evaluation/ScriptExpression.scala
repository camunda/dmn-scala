package org.camunda.dmn.evaluation

import org.camunda.feel.syntaxtree.{Exp, ParsedExpression}

case class ScriptExpression(script: String, language: String) extends ParsedExpression(text = script, expression = null) {
}