package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.camunda.dmn.DmnEngine._

class InvocationTest extends FlatSpec with Matchers with DecisionTest {
  
  lazy val discountDecision = parse("/invocation/discount.dmn")
  lazy val withoutParameters = parse("/invocation/withoutParameters.dmn")
  lazy val missingParameter = parse("/invocation/missingParameters.dmn")
  lazy val missingKnowledgeRequirement = parse("/invocation/missingKnowledgeRequirement.dmn")
    
  "An invocation" should "execute a BKM with parameters" in
  {
    eval(discountDecision, "discount", Map("Customer" -> "Business", "OrderSize" -> 7)) should be(Result(0.1)) 
  }
  
  it should "execute a BKM without parameters" in
  {
    eval(withoutParameters, "applicantData", Map()) should be(
        Result(Map(
                   "Age" -> 51,
                   "MaritalStatus" -> "M", 
                   "EmploymentStatus" -> "EMPLOYED", 
                   "ExistingCustomer" -> false
                   )))
  }
  
  it should "fail if parameter is not set" in 
  {
    engine.eval(missingParameter, "discount", Map("OrderSize" -> 7)) should be(Left(Failure("no parameter found with name 'customer'")))   
  }
  
  it should "fail if knowledge requirement is missing" in 
  {
    engine.eval(missingKnowledgeRequirement, "discount", Map("Customer" -> "Business", "OrderSize" -> 7)) should be(Left(Failure("no BKM found with name 'Discount table'")))   
  }
  
}