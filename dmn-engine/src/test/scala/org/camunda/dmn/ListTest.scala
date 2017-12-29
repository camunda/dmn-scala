package org.camunda.dmn

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.camunda.dmn.DmnEngine._

class ListTest extends FlatSpec with Matchers with DecisionTest {
  
  lazy val applicantData = parse("/list/ApplicantData.dmn")
    
  "A list with literal expressions" should "return result as list" in
  {
    eval(applicantData, "applicantData", Map()) should be(
        Result(Map("MonthlyOutgoings" -> List(2500, 3000)))) 
  }
    
}