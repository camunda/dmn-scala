package org.camunda.dmn

import org.camunda.dmn.parser.InMemoryDmnRepository
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DmnImportTest extends AnyFlatSpec with Matchers with DecisionTest{

  override val engine = new DmnEngine(
    auditLogListeners = List(new TestAuditLogListener),
    dmnRepository = new InMemoryDmnRepository()
  )

  private val importedDecision = parse("/tck/0086-import/Imported_Model.dmn")
  private val importingDecision = parse("/tck/0086-import/0086-import.dmn")

  "A decision with an imported BKM" should "invoke the BKM from the imported DMN (1)" in {
    eval(importingDecision,
      "decision_with_imported_bkm",
      Map("A_Person" -> Map("name" -> "John Doe", "age" -> 21))) should be("Hello John Doe!")
  }

  it should "invoke the BKM from the imported DMN (2)" in {
    eval(importingDecision,
      "decision_with_imported_bkm",
      Map("A_Person" -> Map("name" -> "John Doe", "age" -> 47))) should be("Respectfully, Hello John Doe!")
  }

}
