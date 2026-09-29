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
package org.camunda.dmn.evaluation

import java.time.{Duration, LocalDateTime, Period}

import org.camunda.dmn.DmnEngine.Failure
import org.camunda.feel.syntaxtree._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypeCheckerTest extends AnyFlatSpec with Matchers {

  private val dateTime = ValLocalDateTime(LocalDateTime.parse("2022-01-01T10:00:00"))
  private val yearMonthDuration = ValYearMonthDuration(Period.parse("P1Y2M"))
  private val dayTimeDuration = ValDayTimeDuration(Duration.parse("P1DT2H"))

  "isOfType" should "accept 'dateTime' as typeRef for a date-time value" in {
    TypeChecker.isOfType(dateTime, "dateTime") should be(Right(dateTime))
  }

  it should "accept 'date and time' as typeRef for a date-time value" in {
    TypeChecker.isOfType(dateTime, "date and time") should be(Right(dateTime))
  }

  it should "accept 'yearMonthDuration' as typeRef for a year-month-duration value" in {
    TypeChecker.isOfType(yearMonthDuration, "yearMonthDuration") should be(
      Right(yearMonthDuration))
  }

  it should "accept 'years and months duration' as typeRef for a year-month-duration value" in {
    TypeChecker.isOfType(yearMonthDuration, "years and months duration") should be(
      Right(yearMonthDuration))
  }

  it should "accept 'dayTimeDuration' as typeRef for a day-time-duration value" in {
    TypeChecker.isOfType(dayTimeDuration, "dayTimeDuration") should be(Right(dayTimeDuration))
  }

  it should "accept 'days and time duration' as typeRef for a day-time-duration value" in {
    TypeChecker.isOfType(dayTimeDuration, "days and time duration") should be(
      Right(dayTimeDuration))
  }

  it should "report the given typeRef in the failure message" in {
    TypeChecker.isOfType(ValString("foo"), "date and time") should be(
      Left(Failure(s"expected 'date and time' but found '${ValString("foo")}'")))
  }

  it should "report the given typeRef in the failure message for 'years and months duration'" in {
    TypeChecker.isOfType(ValString("foo"), "years and months duration") should be(
      Left(Failure(s"expected 'years and months duration' but found '${ValString("foo")}'")))
  }

  it should "report the given typeRef in the failure message for 'days and time duration'" in {
    TypeChecker.isOfType(ValString("foo"), "days and time duration") should be(
      Left(Failure(s"expected 'days and time duration' but found '${ValString("foo")}'")))
  }

}
