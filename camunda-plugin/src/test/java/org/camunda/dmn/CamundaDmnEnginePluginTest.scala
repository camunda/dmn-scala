package org.camunda.dmn

import org.scalatest._
import scala.collection.JavaConverters._
import org.camunda.bpm.engine.ProcessEngineConfiguration
import org.camunda.bpm.engine.impl.cfg.ProcessEngineConfigurationImpl
import java.util.{Map => JMap}
import java.math.MathContext

class CamundaDmnEnginePluginTest extends FlatSpec with Matchers with BeforeAndAfter {

  val processEngine = ProcessEngineConfiguration
		.createProcessEngineConfigurationFromResource("default-dmn-config.cfg.xml")
		.buildProcessEngine

  val repositoryService = processEngine.getRepositoryService
	val decisionService = processEngine.getDecisionService
	val runtimeService = processEngine.getRuntimeService

	repositoryService.createDeployment()
		.addClasspathResource("discount.dmn")
		.addClasspathResource("holidays.dmn")
		.addClasspathResource("adjustments.dmn")
		.addClasspathResource("routingRules.dmn")
		.addClasspathResource("loan-comparison.dmn")
		.addClasspathResource("lending.dmn")
		.addClasspathResource("process.bpmn")
		.deploy()

 after {
    processEngine.close
	}
 
  "The repository service" should "found deployed decision" in {
    
    val decision = repositoryService.createDecisionDefinitionQuery()
      .decisionDefinitionKey("discount")
      .singleResult()
      
    decision should not be (null)
    decision.getKey should be ("discount")
    decision.getName should be ("Discount")
    decision.getCategory should be ("camunda")
  }
  
  it should "found deployed decision requirements graph" in {
    
    val drg = repositoryService.createDecisionRequirementsDefinitionQuery()
      .decisionRequirementsDefinitionKey("lending")
      .singleResult()
      
    drg should not be (null)
    drg.getKey should be ("lending")
    drg.getName should be ("Lending")
    drg.getCategory should be ("camunda")
    
    val decisions = repositoryService.createDecisionDefinitionQuery()
      .decisionRequirementsDefinitionId(drg.getId)
      .list()
      .asScala
      .map(_.getKey)
      
    decisions should contain ("d_RequiredMonthlyInstallment")
  }
  
  "The decision service'" should "evaluate a decision table with single result" in {

		val result = decisionService.evaluateDecisionByKey("discount")
		  .variables(Map("customer" -> "Business", "orderSize" -> 7).asJava.asInstanceOf[JMap[String,Object]])
		  .evaluate()

		result.getSingleEntry[Any] should be(0.1)
	}
  
  it should "evaluate a decision table with list result" in {

		val result = decisionService.evaluateDecisionByKey("holidays")
		  .variables(Map("age" -> 58, "yearsOfService" -> 31).asJava.asInstanceOf[JMap[String,Object]])
		  .evaluate()

		result.collectEntries("Holidays") should be(List(22,5,3).asJava)
	}
  
  it should "evaluate a decision table with compound result" in {

		val result = decisionService.evaluateDecisionByKey("adjustments")
		  .variables(Map("customer" -> "Business", "orderSize" -> 7).asJava.asInstanceOf[JMap[String,Object]])
		  .evaluate()

		result.getSingleResult.getEntry[Any]("discount") should be(0.1)
		result.getSingleResult.getEntry[Any]("shipping") should be("Air")
	}
  
  it should "evaluate a decision table with multi-hit compound results" in {

		val result = decisionService.evaluateDecisionByKey("routingRules")
		  .variables(Map("age" -> 25, "riskCategory" -> "MEDIUM", "deptReview" -> true).asJava.asInstanceOf[JMap[String,Object]])
		  .evaluate()
		
		result.get(0).getEntry[Any]("routing") should be("REFER")
		result.get(0).getEntry[Any]("reviewLevel") should be("LEVEL 2")
		result.get(0).getEntry[Any]("reason") should be("Applicant under dept review")
		
		result.get(1).getEntry[Any]("routing") should be("ACCEPT")
		result.get(1).getEntry[Any]("reviewLevel") should be("NONE")
		result.get(1).getEntry[Any]("reason") should be("Acceptable")
	}

  it should "evaluate a decision table no matching rules" in {

		val result = decisionService.evaluateDecisionByKey("adjustments")
		  .variables(Map("customer" -> "Something else", "orderSize" -> 9).asJava.asInstanceOf[JMap[String,Object]])
		  .evaluate()

		result should be(null)
	}
  
  it should "evaluate an BKM invocation" in {

    val variables = Map("RequestedProduct" -> Map(
        "ProductType" -> "STANDARD LOAN",
        "Amount" -> 350000,
        "Rate" -> 0.0395,
        "Term" -> 360))
    
		val result = decisionService.evaluateDecisionByKey("d_RequiredMonthlyInstallment")
		  .variables(variables.asJava.asInstanceOf[JMap[String,Object]])
		  .evaluate()

		result.getSingleEntry[Any] should be(BigDecimal("1680.880325608634796819003637073109"))
	}
  
  it should "evaluate a decision by id" in {

    val decisionDefinition = repositoryService.createDecisionDefinitionQuery()
      .decisionDefinitionKey("discount")
      .singleResult()
    
		val result = decisionService.evaluateDecisionById(decisionDefinition.getId)
		  .variables(Map("customer" -> "Business", "orderSize" -> 7).asJava.asInstanceOf[JMap[String,Object]])
		  .evaluate()

		result.getSingleEntry[Any] should be(0.1)
	}  
  
  "A business rule task" should "invoke a decision table" in {
    
    val processInstance = runtimeService.startProcessInstanceByKey("process", 
        Map("decisionKey" -> "discount", "customer" -> "Business", "orderSize" -> 7).asJava.asInstanceOf[JMap[String,Object]])
    
    runtimeService.getVariable(processInstance.getId, "result") should be(0.1)    
  }
  
}
