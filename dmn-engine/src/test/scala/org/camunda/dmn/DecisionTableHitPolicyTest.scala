package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DecisionTableHitPolicyTest
    extends AnyFlatSpec
    with Matchers
    with DecisionTest {

  lazy val discountDecision = parse("/decisiontable/discount.dmn")
  lazy val routingRulesDecision = parse("/decisiontable/routingRules.dmn")
  lazy val applicantRiskRatingDecision = parse(
    "/decisiontable/applicantRiskRating.dmn")
  lazy val personLoanComplianceDecision = parse(
    "/decisiontable/personLoanCompliance.dmn")
  lazy val applicantRiskRatingPriorityDecision = parse(
    "/decisiontable/applicantRiskRating_priority.dmn")
  lazy val specialDiscountDecision = parse("/decisiontable/specialDiscount.dmn")
  lazy val holidaysCollectSumDecision = parse(
    "/decisiontable/holidays_collect_sum.dmn")
  lazy val discountCollectMaxDecision = parse(
    "/decisiontable/discount_collect_max.dmn")
  lazy val insuranceFeeDecision = parse("/decisiontable/insuranceFee.dmn")
  lazy val holidaysCollectDecision = parse(
    "/decisiontable/holidays_collect.dmn")
  lazy val holidaysOutputOrderDecision = parse(
    "/decisiontable/holidays_output_order.dmn")
  lazy val eligibilityDecision = parse(
    "/decisiontable/studentFinancialPackageEligibility.dmn")

  "The decision table 'Discount' (Unique)" should "return '0.1'" in {
    eval(discountDecision,
         "discount",
         Map("customer" -> "Business", "orderSize" -> 7)) should be(Result(0.1))
  }

  it should "return '0.15'" in {
    eval(discountDecision,
         "discount",
         Map("customer" -> "Business", "orderSize" -> 15)) should be(
      Result(0.15))
  }

  it should "return '0.05'" in {
    eval(discountDecision,
         "discount",
         Map("customer" -> "Private", "orderSize" -> 9)) should be(Result(0.05))
  }

  "The decision table 'Routing Rules' (Output Order)" should "return all values" in {
    val context =
      Map("age" -> 17, "riskCategory" -> "HIGH", "deptReview" -> true)

    eval(routingRulesDecision, "routingRules", context) should be(
      Result(List(
        Map("routing" -> "DECLINE",
            "reviewLevel" -> "NONE",
            "reason" -> "Applicant too young"),
        Map("routing" -> "REFER",
            "reviewLevel" -> "LEVEL 2",
            "reason" -> "Applicant under dept review"),
        Map("routing" -> "REFER",
            "reviewLevel" -> "LEVEL 1",
            "reason" -> "High risk application"),
        Map("routing" -> "ACCEPT",
            "reviewLevel" -> "NONE",
            "reason" -> "Acceptable")
      )))
  }

  it should "return two values" in {
    val context =
      Map("age" -> 25, "riskCategory" -> "MEDIUM", "deptReview" -> true)

    eval(routingRulesDecision, "routingRules", context) should be(
      Result(List(
        Map("routing" -> "REFER",
            "reviewLevel" -> "LEVEL 2",
            "reason" -> "Applicant under dept review"),
        Map("routing" -> "ACCEPT",
            "reviewLevel" -> "NONE",
            "reason" -> "Acceptable")
      )))
  }

  it should "return single value" in {
    val context =
      Map("age" -> 25, "riskCategory" -> "MEDIUM", "deptReview" -> false)

    eval(routingRulesDecision, "routingRules", context) should be(
      Result(
        List(Map("routing" -> "ACCEPT",
                 "reviewLevel" -> "NONE",
                 "reason" -> "Acceptable"))))
  }

  "The decision table 'Applicant Risk Rating' (Unique)" should "return 'Medium'" in {
    eval(applicantRiskRatingDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 65, "medicalHistory" -> "good")) should be(
      Result("Medium"))
    eval(applicantRiskRatingDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 40, "medicalHistory" -> "bad")) should be(
      Result("Medium"))
    eval(applicantRiskRatingDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 22, "medicalHistory" -> "bad")) should be(
      Result("Medium"))
  }

  it should "return 'High'" in {
    eval(applicantRiskRatingDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 65, "medicalHistory" -> "bad")) should be(
      Result("High"))
  }

  it should "return 'Low'" in {
    eval(applicantRiskRatingDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 22, "medicalHistory" -> "good")) should be(
      Result("Low"))
  }

  "The decision table 'Person Loan Compliance' (Any)" should "return 'Not Compliant'" in {
    val context = Map("creditRating" -> "B",
                      "creditBalance" -> 10000,
                      "loanBalance" -> 50000)

    eval(personLoanComplianceDecision, "personLoanCompliance", context) should be(
      Result("Not Compliant"))
  }

  it should "return 'Compliant'" in {
    val context = Map("creditRating" -> "A",
                      "creditBalance" -> 5000,
                      "loanBalance" -> 10000)

    eval(personLoanComplianceDecision, "personLoanCompliance", context) should be(
      Result("Compliant"))
  }

  "The decision table 'Applicant Risk Rating' (Priority)" should "return 'High'" in {
    eval(applicantRiskRatingPriorityDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 65, "medicalHistory" -> "bad")) should be(
      Result("High"))
  }

  it should "return 'Medium'" in {
    eval(applicantRiskRatingPriorityDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 55, "medicalHistory" -> "bad")) should be(
      Result("Medium"))
    eval(applicantRiskRatingPriorityDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 30, "medicalHistory" -> "good")) should be(
      Result("Medium"))
    eval(applicantRiskRatingPriorityDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 20, "medicalHistory" -> "bad")) should be(
      Result("Medium"))
  }

  it should "return 'Low'" in {
    eval(applicantRiskRatingPriorityDecision,
         "applicantRiskRating",
         Map("applicantAge" -> 20, "medicalHistory" -> "good")) should be(
      Result("Low"))
  }

  "The decision table 'Special Discount' (First)" should "return '0'" in {
    val context = Map("typeOfOrder" -> "Web",
                      "customerLocation" -> "Non-US",
                      "typeOfCustomer" -> "Retailer")

    eval(specialDiscountDecision, "specialDiscount", context) should be(
      Result(0))
  }

  it should "return '5'" in {
    val context = Map("typeOfOrder" -> "Web",
                      "customerLocation" -> "US",
                      "typeOfCustomer" -> "Retailer")

    eval(specialDiscountDecision, "specialDiscount", context) should be(
      Result(5))
  }

  it should "return '10'" in {
    val context = Map("typeOfOrder" -> "Web",
                      "customerLocation" -> "US",
                      "typeOfCustomer" -> "Wholesaler")

    eval(specialDiscountDecision, "specialDiscount", context) should be(
      Result(10))
  }

  "The decision table 'Holidays' (Collect-Sum)" should "return '30'" in {
    eval(holidaysCollectSumDecision,
         "holidays",
         Map("age" -> 58, "yearsOfService" -> 31)) should be(Result(30))
  }

  it should "return '22'" in {
    eval(holidaysCollectSumDecision,
         "holidays",
         Map("age" -> 25, "yearsOfService" -> 2)) should be(Result(22))
  }

  it should "return '27'" in {
    eval(holidaysCollectSumDecision,
         "holidays",
         Map("age" -> 16, "yearsOfService" -> 1)) should be(Result(27))
  }

  it should "return '24'" in {
    eval(holidaysCollectSumDecision,
         "holidays",
         Map("age" -> 46, "yearsOfService" -> 19)) should be(Result(24))
  }

  "The decision table 'Discount' (Collect-Max)" should "return '0.1'" in {
    eval(discountCollectMaxDecision,
         "discount",
         Map("customer" -> "Business", "orderSize" -> 8)) should be(Result(0.1))
  }

  it should "return '0.15'" in {
    eval(discountCollectMaxDecision,
         "discount",
         Map("customer" -> "Business", "orderSize" -> 12)) should be(
      Result(0.15))
  }

  it should "return '0.06'" in {
    eval(discountCollectMaxDecision,
         "discount",
         Map("customer" -> "Private", "orderSize" -> 17)) should be(
      Result(0.06))
  }

  it should "return '0.05'" in {
    eval(discountCollectMaxDecision,
         "discount",
         Map("customer" -> "Private", "orderSize" -> 13)) should be(
      Result(0.05))
  }

  "The decision table 'Insurance Fee' (Collect-Min)" should "return '200'" in {
    eval(insuranceFeeDecision, "insuranceFee", Map("years" -> 1)) should be(
      Result(200))
  }

  it should "return '190'" in {
    eval(insuranceFeeDecision, "insuranceFee", Map("years" -> 4)) should be(
      Result(190))
  }

  it should "return '100'" in {
    eval(insuranceFeeDecision, "insuranceFee", Map("years" -> 16)) should be(
      Result(100))
  }

  "The decision table 'Holidays' (Collect)" should "return '22,3,5'" in {
    eval(holidaysCollectDecision,
         "holidays",
         Map("age" -> 58, "yearsOfService" -> 31)) should be(
      Result(List(22, 3, 5)))
  }

  it should "return '22'" in {
    eval(holidaysCollectDecision,
         "holidays",
         Map("age" -> 25, "yearsOfService" -> 2)) should be(Result(List(22)))
  }

  "The decision table 'Holidays' (Output Order)" should "return '22,5,3'" in {
    eval(holidaysOutputOrderDecision,
         "holidays",
         Map("age" -> 58, "yearsOfService" -> 31)) should be(
      Result(List(22, 5, 3)))
  }

  it should "return '22'" in {
    eval(holidaysOutputOrderDecision,
         "holidays",
         Map("age" -> 25, "yearsOfService" -> 2)) should be(Result(List(22)))
  }

  it should "return '22,5'" in {
    eval(holidaysOutputOrderDecision,
         "holidays",
         Map("age" -> 16, "yearsOfService" -> 1)) should be(Result(List(22, 5)))
  }

  it should "return '22,2,2'" in {
    eval(holidaysOutputOrderDecision,
         "holidays",
         Map("age" -> 46, "yearsOfService" -> 19)) should be(
      Result(List(22, 2, 2)))
  }

  it should "return '22,2'" in {
    eval(holidaysOutputOrderDecision,
         "holidays",
         Map("age" -> 40, "yearsOfService" -> 19)) should be(
      Result(List(22, 2)))
  }

  "The decision table 'Student Financial Package Eligibility' (Rule Order)" should "return '20% Scholarship, 30% Loan'" in {
    val context =
      Map("gpa" -> 3.6, "acitvitiesCount" -> 4, "socialMembership" -> "Yes")

    eval(eligibilityDecision, "eligibility", context) should be(
      Result(List("20% Scholarship", "30% Loan")))
  }

  it should "return '20% Work-On-Campus'" in {
    val context =
      Map("gpa" -> 3.6, "acitvitiesCount" -> 4, "socialMembership" -> "No")

    eval(eligibilityDecision, "eligibility", context) should be(
      Result(List("20% Work-On-Campus")))
  }

  it should "return '5% Work-On-Campus'" in {
    val context =
      Map("gpa" -> 3.0, "acitvitiesCount" -> 4, "socialMembership" -> "Yes")

    eval(eligibilityDecision, "eligibility", context) should be(
      Result(List("5% Work-On-Campus")))
  }

}
