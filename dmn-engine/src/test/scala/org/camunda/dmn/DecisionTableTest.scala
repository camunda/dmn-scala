package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DecisionTableTest extends FlatSpec with Matchers with DecisionTest {
  
  lazy val discountDecision = parse("/discount.dmn")
  lazy val discountWithDefaultOutputDecision = parse("/discount_default-output.dmn")
  
  lazy val adjustmentsDecision = parse("/adjustments.dmn")
  lazy val adjustmentsWithDefaultOutputDecision = parse("/adjustments_default-output.dmn")
  
  lazy val routingRulesDecision = parse("/routingRules.dmn")    
  lazy val holidaysDecision = parse("/holidays_output_order.dmn")
  
  
  "A decision table with single output" should "return single value" in
  {
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 7)) should be(EvalValue(0.1)) 
  }
  
  it should "return value list" in
  {
    engine.eval(holidaysDecision, "holidays", Map("age" -> 58, "yearsOfService" -> 31)) should be(EvalValue(List(22,5,3)))
  }
  
  it should "return null if no rule match" in 
  {
    engine.eval(discountDecision, "discount", Map("customer" -> "Something else", "orderSize" -> 9)) should be(EvalNull)  
  }
  
  it should "return the default-output if no rule match" in 
  {
    engine.eval(discountWithDefaultOutputDecision, "discount", Map("customer" -> "Something else", "orderSize" -> 9)) should be(EvalValue(0.05))  
  }
  
  
  "A decision table with multiple outputs" should "return single values" in
  {
    val context = Map("customer" -> "Business", "orderSize" -> 7)
    
    engine.eval(adjustmentsDecision, "adjustments", context) should be(EvalValue(Map("discount" -> 0.1, "shipping" -> "Air"))) 
  }
  
  it should "return value list" in 
  {
    val context = Map("age" -> 25, "riskCategory" -> "MEDIUM", "deptReview" -> true)
    
    engine.eval(routingRulesDecision, "routingRules", context) should be(EvalValue(List(
      Map("routing" -> "REFER",   "reviewLevel" -> "LEVEL 2",  "reason" -> "Applicant under dept review"),
      Map("routing" -> "ACCEPT",  "reviewLevel" -> "NONE",     "reason" -> "Acceptable")
    )))  
  }
  
  it should "return null if no rule match" in 
  {
    val context = Map("customer" -> "Something else", "orderSize" -> 9)
    
    engine.eval(adjustmentsDecision, "adjustments", context) should be(EvalNull)  
  }
  
  it should "return the default-output if no rule match" in 
  {
    val context = Map("customer" -> "Something else", "orderSize" -> 9)
    
    engine.eval(adjustmentsWithDefaultOutputDecision, "adjustments", context) should be(EvalValue(Map("discount" -> 0.05, "shipping" -> "Ground")))  
  }
 
  
}