package org.camunda.dmn.standalone

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.camunda.dmn.DmnEngine._
import java.nio.file.Paths
import org.scalatest.BeforeAndAfter
import java.nio.file.Files
import java.io.FileInputStream

class StandaloneEngineTest extends FlatSpec with Matchers with BeforeAndAfter {
  
	val repository = Paths.get(getClass.getResource("/repository").toURI()).toString
	val emptyRepository = Paths.get(getClass.getResource("/empty").toURI()).toString

	after {
	  Files.list(Paths.get(emptyRepository)).forEach(p => Files.delete(p))
	}
	
	"A standalone engine" should "scan directory" in 
	{
    val engine = StandaloneEngine.fileSystemRepository(repository)
    
    engine.getDecisions.map(_.resource) should contain allOf ("loan-comparison.dmn", "discount.dmn")
  }
	
	it should "return all decisions" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(repository)
    
    engine.getDecisions should have size 3
    engine.getDecisions.map(_.decisionId) should contain allOf ("discount", "bankrates", "rankedProducts")
	}
	
	it should "evaluate decision by id" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(repository)
    
    engine.evalDecisionById("discount", Map("customer" -> "Business", "orderSize" -> 7)) should be (Right(Result(0.1)))
	}
	
	it should "evaluate decision by name" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(repository)
    
    engine.evalDecisionByName("Discount", Map("customer" -> "Business", "orderSize" -> 7)) should be (Right(Result(0.1)))
	}
	
	it should "return empty list if repository is empty" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(emptyRepository)
    
    engine.getDecisions should be (List.empty)
	}
	
	it should "fail to evaluate decision by not existing id" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(emptyRepository)
    
    engine.evalDecisionById("discount", Map()) should be (Left(Failure("No decision found with id 'discount'")))
	}
	
	it should "fail to evaluate decision by not existing name" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(emptyRepository)
    
    engine.evalDecisionByName("Discount", Map()) should be (Left(Failure("No decision found with name 'Discount'")))
	}
	
	it should "report evaluation failure" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(repository)
    
    engine.evalDecisionById("discount", Map("orderSize" -> 7)) should be (Left(
        Failure("failed to evaluate expression 'customer':\nno variable found for name 'customer'")))
	}
	
	it should "insert DMN resource" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(emptyRepository)
	  
	  val stream = getClass.getResourceAsStream("/repository/discount.dmn")
	  
	  val result = engine.insertDecisions(stream, "discount.dmn")
	  val deployedDecision = result.right.get.head 
	  
	  deployedDecision.decisionId should be ("discount")
	  deployedDecision.resource should be ("discount.dmn")
	}
	
	it should "remove DMN resource" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(emptyRepository)
	  
	  val stream = getClass.getResourceAsStream("/repository/discount.dmn")
	  
	  engine.insertDecisions(stream, "discount.dmn")
	  
	  val result = engine.removeResource("discount.dmn")
	  val removedDecision = result.right.get.head
	  
	  removedDecision.decisionId should be ("discount")
	  
	  engine.getDecisions should be (List())
	}
	
	it should "fail to remove not existing resource" in 
	{
	  val engine = StandaloneEngine.fileSystemRepository(emptyRepository)
	  
	  engine.removeResource("discount.dmn") should be (Left(Failure("No decisions found for resource 'discount.dmn'")))
	}
  
}