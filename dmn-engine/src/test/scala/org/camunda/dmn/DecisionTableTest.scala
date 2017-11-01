package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DecisionTableTest extends FlatSpec with Matchers {
  
  val engine = new DmnEngine
  
  lazy val discountDecision = {
    val stream = getClass.getResourceAsStream("/discount.dmn")
    
    engine.parse(stream).left.get
  }
  
  "A decision table with unique hit policy" should "return a value if one rule match" in {
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 7)) should be(EvalValue(0.1))    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 15)) should be(EvalValue(0.15))    
    engine.eval(discountDecision, "discount", Map("customer" -> "Private", "orderSize" -> 9)) should be(EvalValue(0.05))
  }
  
  it should "return null if no rule match" in {
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Something else", "orderSize" -> 9)) should be(EvalNull)  
  }
  
}