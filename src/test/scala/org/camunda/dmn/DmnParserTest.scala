package org.camunda.dmn

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnParserTest extends AnyFlatSpec with Matchers with DecisionTest {

  "A DMN file with cyclic dependencies between decisions" should "return an error" in {
    val failure =
      parseDmn("/requirements/cyclic-dependencies-in-decisions.dmn").failure

    failure.message should be(
      "Invalid DMN model: Cyclic dependencies between decisions detected.")
  }

}
