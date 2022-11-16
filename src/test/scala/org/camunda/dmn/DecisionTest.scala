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
