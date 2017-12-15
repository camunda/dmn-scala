package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.camunda.dmn.DmnEngine._

class InvocationTest extends FlatSpec with Matchers with DecisionTest {
  
  lazy val discountDecision = parse("/invocation/discount.dmn")
    
  "An invocation" should "execute a decision table" in
  {
    eval(discountDecision, "discount", Map("Customer" -> "Business", "OrderSize" -> 7)) should be(Result(0.1)) 
  }
  
}