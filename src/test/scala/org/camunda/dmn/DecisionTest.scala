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

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.ParsedDmn
import org.camunda.dmn.Audit.AuditLog
import org.camunda.dmn.Audit.AuditLogListener

import java.io.InputStream

trait DecisionTest {

  val engine = new DmnEngine(auditLogListeners = List(new TestAuditLogListener))

  def parse(file: String): ParsedDmn = {
    parseDmn(file).dmn
  }

  def parseDmn(dmn: String): ParsedResult = {
    val stream = getClass.getResourceAsStream(dmn)
    new ParsedResult(engine.parse(stream))
  }

  class ParsedResult(val parserResult: Either[Failure, ParsedDmn]) {
    def failure: Failure = {
      parserResult match {
        case Right(_) =>
          throw new AssertionError(
            "Expected parsing to fail, but was successful")
        case Left(failure) => failure
      }
    }

    def dmn: ParsedDmn = {
      parserResult match {
        case Right(dmn)    => dmn
        case Left(failure) => throw new AssertionError(failure.message)
      }
    }
  }

  def eval(decision: ParsedDmn, id: String, context: Map[String, Any]): Any =
    engine.eval(decision, id, context) match {
      case Right(result)                 => result.value
      case Left(EvalFailure(failure, _)) => failure
    }

  var lastAuditLog: AuditLog = _

  class TestAuditLogListener extends AuditLogListener {
    override def onEval(log: AuditLog) = {
      lastAuditLog = log
    }
    override def onFailure(log: AuditLog) = {
      lastAuditLog = log
    }
  }

  def auditLog = lastAuditLog

}
