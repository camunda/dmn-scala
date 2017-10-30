package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DmnEngineTest extends FlatSpec with Matchers {
  
  val engine = new DmnEngine
  
  lazy val discountDecision = {
    val stream = getClass.getResourceAsStream("/discount.dmn")
    
    engine.parse(stream).left.get
  }
  
  "A DMN engine" should "evaluate a decision table" in {
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 7)) should be(EvalValue(0.1))
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 15)) should be(EvalValue(0.15))
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Private", "orderSize" -> 9)) should be(EvalValue(0.05))
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Something else", "orderSize" -> 9)) should be(EvalNull)
  }
  
}