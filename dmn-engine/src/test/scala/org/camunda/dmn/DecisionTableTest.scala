package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DecisionTableTest extends FlatSpec with Matchers {
  
  val engine = new DmnEngine
  
  private def parse(file: String): ParsedDmn = {
    val stream = getClass.getResourceAsStream(file)    
    val result = engine.parse(stream)
    
    if (result.isRight) {
      println(result.right.get.mkString("\n"))
    }
    
    result.left.get
  }
  
  lazy val discountDecision = parse("/discount.dmn")    
  lazy val routingRulesDecision = parse("/routingRules.dmn")    
  
  "A decision table with no matching rule" should "return null" in {
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Something else", "orderSize" -> 9)) should be(EvalNull)  
  }

  "A decision table with single hit policy" should "return single value" in {
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 7)) should be(EvalValue(0.1)) 
  }
  
  "A decision table with hit policy UNIQUE" should "return single value" in {
    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 7)) should be(EvalValue(0.1))    
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 15)) should be(EvalValue(0.15))    
    engine.eval(discountDecision, "discount", Map("customer" -> "Private", "orderSize" -> 9)) should be(EvalValue(0.05))
  }
  
  "A decision table with hit policy OUTPUT ORDER" should "return all values by priority" in {
    
    val context = Map("age" -> 17, "riskCategory" -> "HIGH", "deptReview" -> true)
    
    engine.eval(routingRulesDecision, "routingRules", context) should be(EvalValue(List(
      Map("routing" -> "DECLINE", "reviewLevel" -> "NONE",     "reason" -> "Applicant too young"),
      Map("routing" -> "REFER",   "reviewLevel" -> "LEVEL 2",  "reason" -> "Applicant under dept review"),
      Map("routing" -> "REFER",   "reviewLevel" -> "LEVEL 1",  "reason" -> "High risk application"),
      Map("routing" -> "ACCEPT",  "reviewLevel" -> "NONE",     "reason" -> "Acceptable")
    )))  
  }
  
  
}