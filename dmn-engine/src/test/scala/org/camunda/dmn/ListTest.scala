package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ListTest extends AnyFlatSpec with Matchers with DecisionTest {

  lazy val applicantData = parse("/list/ApplicantData.dmn")

  "A list with literal expressions" should "return result as list" in {
    eval(applicantData, "applicantData", Map()) should be(
      Result(Map("MonthlyOutgoings" -> List(2500, 3000))))
  }

}
