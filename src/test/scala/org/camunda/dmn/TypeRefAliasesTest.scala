/*
 * Copyright © 2022 Camunda Services GmbH (info@camunda.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.camunda.dmn

import java.time.{Duration, LocalDateTime, Period}

import org.camunda.dmn.DmnEngine._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeRefAliasesTest extends AnyFlatSpec with Matchers with DecisionTest {

  private lazy val typeRefAliases = parse(
    "/literalexpression/type-ref-aliases.dmn")

  "A decision with typeRef 'date and time'" should "accept a matching date-time result" in {
    eval(typeRefAliases, "dateAndTime", Map()) should be(
      LocalDateTime.parse("2022-01-01T10:00:00"))
  }

  it should "fail when the result doesn't match the type" in {
    eval(typeRefAliases, "dateAndTimeMismatch", Map()) should be(
      Failure("expected 'date and time' but found '\"not a date\"'"))
  }

  "A decision with typeRef 'years and months duration'" should "accept a matching duration result" in {
    eval(typeRefAliases, "yearsAndMonthsDuration", Map()) should be(
      Period.parse("P1Y2M"))
  }

  "A decision with typeRef 'days and time duration'" should "accept a matching duration result" in {
    eval(typeRefAliases, "daysAndTimeDuration", Map()) should be(
      Duration.parse("P1DT2H"))
  }

}
