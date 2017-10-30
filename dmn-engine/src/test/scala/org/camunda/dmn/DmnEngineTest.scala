package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DmnEngineTest extends FlatSpec with Matchers {
  
  val engine = new DmnEngine
  
  def discountDecision = getClass.getResourceAsStream("/discount.dmn")
  
  "A DMN engine" should "evaluate a decision table" in {
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 7)) should be(EvalValue("discount", 0.1))
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 15)) should be(EvalValue("discount", 0.15))
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Private", "orderSize" -> 9)) should be(EvalValue("discount", 0.05))
  }
  
}