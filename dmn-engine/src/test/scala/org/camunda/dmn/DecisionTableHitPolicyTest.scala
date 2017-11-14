package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DecisionTableHitPolicyTest extends FlatSpec with Matchers with DecisionTest {
  
  lazy val discountDecision = parse("/discount.dmn")    
  lazy val routingRulesDecision = parse("/routingRules.dmn")    
  lazy val applicantRiskRatingDecision = parse("/applicantRiskRating.dmn")
  lazy val personLoanComplianceDecision = parse("/personLoanCompliance.dmn")
  lazy val applicantRiskRatingPriorityDecision = parse("/applicantRiskRating_priority.dmn")
  lazy val specialDiscountDecision = parse("/specialDiscount.dmn")
  lazy val holidaysCollectSumDecision = parse("/holidays_collect_sum.dmn")
  lazy val discountCollectMaxDecision = parse("/discount_collect_max.dmn")
  lazy val insuranceFeeDecision = parse("/insuranceFee.dmn")
  lazy val holidaysCollectDecision = parse("/holidays_collect.dmn")
  lazy val holidaysOutputOrderDecision = parse("/holidays_output_order.dmn")
  lazy val eligibilityDecision = parse("/studentFinancialPackageEligibility.dmn")
  
  "The decision table 'Discount' (Unique)" should "return '0.1'" in 
  {
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 7)) should be(EvalValue(0.1))   
  }
  
  it should "return '0.15'" in 
  {
    engine.eval(discountDecision, "discount", Map("customer" -> "Business", "orderSize" -> 15)) should be(EvalValue(0.15))   
  }
  
  it should "return '0.05'" in 
  {
    engine.eval(discountDecision, "discount", Map("customer" -> "Private", "orderSize" -> 9)) should be(EvalValue(0.05)) 
  }
  
  
  "The decision table 'Routing Rules' (Output Order)" should "return all values" in 
  {
    val context = Map("age" -> 17, "riskCategory" -> "HIGH", "deptReview" -> true)
    
    engine.eval(routingRulesDecision, "routingRules", context) should be(EvalValue(List(
      Map("routing" -> "DECLINE", "reviewLevel" -> "NONE",     "reason" -> "Applicant too young"),
      Map("routing" -> "REFER",   "reviewLevel" -> "LEVEL 2",  "reason" -> "Applicant under dept review"),
      Map("routing" -> "REFER",   "reviewLevel" -> "LEVEL 1",  "reason" -> "High risk application"),
      Map("routing" -> "ACCEPT",  "reviewLevel" -> "NONE",     "reason" -> "Acceptable")
    )))  
  }
  
  it should "return two values" in 
  {
    val context = Map("age" -> 25, "riskCategory" -> "MEDIUM", "deptReview" -> true)
    
    engine.eval(routingRulesDecision, "routingRules", context) should be(EvalValue(List(
      Map("routing" -> "REFER",   "reviewLevel" -> "LEVEL 2",  "reason" -> "Applicant under dept review"),
      Map("routing" -> "ACCEPT",  "reviewLevel" -> "NONE",     "reason" -> "Acceptable")
    )))  
  }
  
  it should "return single value" in 
  {
    val context = Map("age" -> 25, "riskCategory" -> "MEDIUM", "deptReview" -> false)
    
    engine.eval(routingRulesDecision, "routingRules", context) should be(EvalValue(
      Map("routing" -> "ACCEPT",  "reviewLevel" -> "NONE",     "reason" -> "Acceptable")
    ))  
  }
  
  
  "The decision table 'Applicant Risk Rating' (Unique)" should "return 'Medium'" in 
  {
    engine.eval(applicantRiskRatingDecision, "applicantRiskRating", Map("applicantAge" -> 65, "medicalHistory" -> "good")) should be(EvalValue("Medium"))
    engine.eval(applicantRiskRatingDecision, "applicantRiskRating", Map("applicantAge" -> 40, "medicalHistory" -> "bad")) should be(EvalValue("Medium"))
    engine.eval(applicantRiskRatingDecision, "applicantRiskRating", Map("applicantAge" -> 22, "medicalHistory" -> "bad")) should be(EvalValue("Medium"))
  }
  
  it should "return 'High'" in 
  {
    engine.eval(applicantRiskRatingDecision, "applicantRiskRating", Map("applicantAge" -> 65, "medicalHistory" -> "bad")) should be(EvalValue("High"))
  }
  
  it should "return 'Low'" in 
  {
    engine.eval(applicantRiskRatingDecision, "applicantRiskRating", Map("applicantAge" -> 22, "medicalHistory" -> "good")) should be(EvalValue("Low"))
  }
  
  
  "The decision table 'Person Loan Compliance' (Any)" should "return 'Not Compliant'" in 
  {
    val context = Map("creditRating" -> "B", "creditBalance" -> 10000, "loanBalance" -> 50000)
    
    engine.eval(personLoanComplianceDecision, "personLoanCompliance", context) should be(EvalValue("Not Compliant"))
  }
  
  it should "return 'Compliant'" in 
  {
    val context = Map("creditRating" -> "A", "creditBalance" -> 5000, "loanBalance" -> 10000)
    
    engine.eval(personLoanComplianceDecision, "personLoanCompliance", context) should be(EvalValue("Compliant"))
  }
  
  
  "The decision table 'Applicant Risk Rating' (Priority)" should "return 'High'" in 
  {
    engine.eval(applicantRiskRatingPriorityDecision, "applicantRiskRating", Map("applicantAge" -> 65, "medicalHistory" -> "bad")) should be(EvalValue("High"))
  }
  
  it should "return 'Medium'" in 
  {
    engine.eval(applicantRiskRatingPriorityDecision, "applicantRiskRating", Map("applicantAge" -> 55, "medicalHistory" -> "bad")) should be(EvalValue("Medium"))
    engine.eval(applicantRiskRatingPriorityDecision, "applicantRiskRating", Map("applicantAge" -> 30, "medicalHistory" -> "good")) should be(EvalValue("Medium"))
    engine.eval(applicantRiskRatingPriorityDecision, "applicantRiskRating", Map("applicantAge" -> 20, "medicalHistory" -> "bad")) should be(EvalValue("Medium"))
  }
  
  it should "return 'Low'" in 
  {
    engine.eval(applicantRiskRatingPriorityDecision, "applicantRiskRating", Map("applicantAge" -> 20, "medicalHistory" -> "good")) should be(EvalValue("Low"))
  }
  
  
  "The decision table 'Special Discount' (First)" should "return '0'" in 
  {
    val context = Map("typeOfOrder" -> "Web", "customerLocation" -> "Non-US", "typeOfCustomer" -> "Retailer")
    
    engine.eval(specialDiscountDecision, "specialDiscount", context) should be(EvalValue(0))
  }
  
  it should "return '5'" in 
  {
    val context = Map("typeOfOrder" -> "Web", "customerLocation" -> "US", "typeOfCustomer" -> "Retailer")
    
    engine.eval(specialDiscountDecision, "specialDiscount", context) should be(EvalValue(5))
  }  
  
  it should "return '10'" in 
  {
    val context = Map("typeOfOrder" -> "Web", "customerLocation" -> "US", "typeOfCustomer" -> "Wholesaler")
    
    engine.eval(specialDiscountDecision, "specialDiscount", context) should be(EvalValue(10))
  }  
  
  
  "The decision table 'Holidays' (Collect-Sum)" should "return '30'" in 
  {
    engine.eval(holidaysCollectSumDecision, "holidays", Map("age" -> 58, "yearsOfService" -> 31)) should be(EvalValue(30))
  }
  
  it should "return '22'" in 
  {
    engine.eval(holidaysCollectSumDecision, "holidays", Map("age" -> 25, "yearsOfService" -> 2)) should be(EvalValue(22))
  }
  
  it should "return '27'" in 
  {
    engine.eval(holidaysCollectSumDecision, "holidays", Map("age" -> 16, "yearsOfService" -> 1)) should be(EvalValue(27))
  }
  
  it should "return '24'" in 
  {
    engine.eval(holidaysCollectSumDecision, "holidays", Map("age" -> 46, "yearsOfService" -> 19)) should be(EvalValue(24))
  }
  
  
  "The decision table 'Discount' (Collect-Max)" should "return '0.1'" in 
  {
    engine.eval(discountCollectMaxDecision, "discount", Map("customer" -> "Business", "orderSize" -> 8)) should be(EvalValue(0.1))
  }
  
  it should "return '0.15'" in 
  {
    engine.eval(discountCollectMaxDecision, "discount", Map("customer" -> "Business", "orderSize" -> 12)) should be(EvalValue(0.15))
  }
  
  it should "return '0.06'" in 
  {
    engine.eval(discountCollectMaxDecision, "discount", Map("customer" -> "Private", "orderSize" -> 17)) should be(EvalValue(0.06))
  }
  
  it should "return '0.05'" in 
  {
    engine.eval(discountCollectMaxDecision, "discount", Map("customer" -> "Private", "orderSize" -> 13)) should be(EvalValue(0.05))
  }
    
  
  "The decision table 'Insurance Fee' (Collect-Min)" should "return '200'" in 
  {
    engine.eval(insuranceFeeDecision, "insuranceFee", Map("years" -> 1)) should be(EvalValue(200))
  }
  
  it should "return '190'" in 
  {
    engine.eval(insuranceFeeDecision, "insuranceFee", Map("years" -> 4)) should be(EvalValue(190))
  }
  
  it should "return '100'" in 
  {
    engine.eval(insuranceFeeDecision, "insuranceFee", Map("years" -> 16)) should be(EvalValue(100))
  }
  
  
  "The decision table 'Holidays' (Collect)" should "return '22,3,5'" in 
  {
    engine.eval(holidaysCollectDecision, "holidays", Map("age" -> 58, "yearsOfService" -> 31)) should be(EvalValue(List(22,3,5)))
  }
  
  it should "return '22'" in 
  {
    engine.eval(holidaysCollectDecision, "holidays", Map("age" -> 25, "yearsOfService" -> 2)) should be(EvalValue(22))
  }
  
  "The decision table 'Holidays' (Output Order)" should "return '22,5,3'" in 
  {
    engine.eval(holidaysOutputOrderDecision, "holidays", Map("age" -> 58, "yearsOfService" -> 31)) should be(EvalValue(List(22,5,3)))
  }
  
  it should "return '22'" in 
  {
    engine.eval(holidaysOutputOrderDecision, "holidays", Map("age" -> 25, "yearsOfService" -> 2)) should be(EvalValue(22))
  }
  
  it should "return '22,5'" in 
  {
    engine.eval(holidaysOutputOrderDecision, "holidays", Map("age" -> 16, "yearsOfService" -> 1)) should be(EvalValue(List(22,5)))
  }
  
  it should "return '22,2,2'" in 
  {
    engine.eval(holidaysOutputOrderDecision, "holidays", Map("age" -> 46, "yearsOfService" -> 19)) should be(EvalValue(List(22,2,2)))
  }
  
  it should "return '22,2'" in 
  {
    engine.eval(holidaysOutputOrderDecision, "holidays", Map("age" -> 40, "yearsOfService" -> 19)) should be(EvalValue(List(22,2)))
  }
  
  
  "The decision table 'Student Financial Package Eligibility' (Rule Order)" should "return '20% Scholarship, 30% Loan'" in 
  {
    val context = Map("gpa" -> 3.6, "acitvitiesCount" -> 4, "socialMembership" -> "Yes")
    
    engine.eval(eligibilityDecision, "eligibility", context) should be(EvalValue(List("20% Scholarship", "30% Loan")))
  }
  
  it should "return '20% Work-On-Campus'" in 
  {
    val context = Map("gpa" -> 3.6, "acitvitiesCount" -> 4, "socialMembership" -> "No")
    
    engine.eval(eligibilityDecision, "eligibility", context) should be(EvalValue("20% Work-On-Campus"))
  }
  
  it should "return '5% Work-On-Campus'" in 
  {
    val context = Map("gpa" -> 3.0, "acitvitiesCount" -> 4, "socialMembership" -> "Yes")
    
    engine.eval(eligibilityDecision, "eligibility", context) should be(EvalValue("5% Work-On-Campus"))
  }
  
}