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

import org.camunda.dmn.DmnEngine._
import org.camunda.feel.syntaxtree._

object TypeChecker {

  val FEEL_PREFIX = "feel:"

  def isOfType(value: Val, typeRef: String): Either[Failure, Val] = {
    if (typeRef == null || typeRef.isEmpty) {
      Right(value)

    } else if (value == ValNull) {
      Right(value)

    } else {
      // accept type with and without prefix
      val `type` = typeRef.replaceAll(FEEL_PREFIX, "")

      `type` match {
        case "string" =>
          value match {
            case v: ValString => Right(v)
            case other        => Left(Failure(s"expected 'string' but found '$other'"))
          }
        case "number" =>
          value match {
            case v: ValNumber => Right(v)
            case other        => Left(Failure(s"expected 'number' but found '$other'"))
          }
        case "boolean" =>
          value match {
            case v: ValBoolean => Right(v)
            case other =>
              Left(Failure(s"expected 'boolean' but found '$other'"))
          }
        case "time" =>
          value match {
            case v: ValTime      => Right(v)
            case v: ValLocalTime => Right(v)
            case other           => Left(Failure(s"expected 'time' but found '$other'"))
          }
        case "date" =>
          value match {
            case v: ValDate => Right(v)
            case other      => Left(Failure(s"expected 'date' but found '$other'"))
          }
        case "dateTime" =>
          value match {
            case v: ValDateTime      => Right(v)
            case v: ValLocalDateTime => Right(v)
            case other =>
              Left(Failure(s"expected 'dateTime' but found '$other'"))
          }
        case "dayTimeDuration" =>
          value match {
            case v: ValDayTimeDuration => Right(v)
            case other =>
              Left(Failure(s"expected 'dayTimeDuration' but found '$other'"))
          }
        case "yearMonthDuration" =>
          value match {
            case v: ValYearMonthDuration => Right(v)
            case other =>
              Left(Failure(s"expected 'yearMonthDuration' but found '$other'"))
          }
        case other => Right(value) // ignore
      }
    }
  }

}
