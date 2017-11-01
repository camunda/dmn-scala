package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DmnEngineTest extends FlatSpec with Matchers {
  
  val engine = new DmnEngine
  
  def discountDecision = getClass.getResourceAsStream("/discount.dmn")
  def invalidExpressionDecision = getClass.getResourceAsStream("/invalid-expression.dmn")
  
  "A DMN engine" should "evaluate a decision table" in {
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 7)) should be(EvalValue(0.1))
  }
  
  it should "parse and evaluate a decision table" in {
    
    val parseResult = engine.parse(discountDecision)
    
    parseResult.isLeft should be(true)
    
    val parsedDmn = parseResult.left.get
    
    engine.eval(parsedDmn, "discount", Map("customer" -> "Business", "orderSize" -> 7)) should be(EvalValue(0.1))
  }
  
  it should "report parse failures" in {
    
    val parseResult = engine.parse(invalidExpressionDecision)
    
    parseResult.isRight should be(true)
    
    val failures = parseResult.right.get
    
    failures.mkString(",") should include ("Failed to parse FEEL unary-tests '>>> 10'")
  }
  
}