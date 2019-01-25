package org.camunda.dmn

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.parser.ParsedDmn
import org.camunda.dmn.Audit.AuditLog
import org.camunda.dmn.Audit.AuditLogListener

trait DecisionTest {

  val engine = new DmnEngine(auditLogListeners = List(new TestAuditLogListener))

  def parse(file: String): ParsedDmn = {
    val stream = getClass.getResourceAsStream(file)
    val result = engine.parse(stream)

    result.left.foreach(println)

    result.right.get
  }

  def eval(decision: ParsedDmn, id: String, context: Map[String, Any]): Any = {
    val result = engine.eval(decision, id, context)

    result.left.foreach(println)

    result.right.get
  }

  var lastAuditLog: AuditLog = _

  class TestAuditLogListener extends AuditLogListener {
    override def onEval(log: AuditLog) = {
      lastAuditLog = log
    }
  }

  def auditLog = lastAuditLog

}
