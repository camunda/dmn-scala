package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.camunda.dmn.DmnEngine._

class LiteralExpressionTest extends FlatSpec with Matchers with DecisionTest {
  
  lazy val greeting = parse("/literalexpression/greeting.dmn")
    
  "A literal expression" should "be evaluated as decision" in
  {
    eval(greeting, "greeting", Map("name" -> "John")) should be(Result("Hello John")) 
  }
    
}