package org.camunda.dmn.camunda.plugin

import java.util.{Map => JMap}

import org.camunda.bpm.engine.ProcessEngineConfiguration
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.JavaConverters._

class CamundaDmnEnginePluginTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfter {

  val processEngine = ProcessEngineConfiguration
    .createProcessEngineConfigurationFromResource("default-dmn-config.cfg.xml")
    .buildProcessEngine

  val repositoryService = processEngine.getRepositoryService
  val decisionService = processEngine.getDecisionService
  val runtimeService = processEngine.getRuntimeService
  val historyService = processEngine.getHistoryService

  repositoryService
    .createDeployment()
    .addClasspathResource("discount.dmn")
    .addClasspathResource("holidays.dmn")
    .addClasspathResource("adjustments.dmn")
    .addClasspathResource("routingRules.dmn")
    .addClasspathResource("loan-comparison.dmn")
    .addClasspathResource("lending.dmn")
    .addClasspathResource("example.dmn")
    .addClasspathResource("process.bpmn")
    .deploy()

  after {
    processEngine.close
  }

  "The repository service" should "found deployed decision" in {

    val decision = repositoryService
      .createDecisionDefinitionQuery()
      .decisionDefinitionKey("discount")
      .singleResult()

    decision should not be (null)
    decision.getKey should be("discount")
    decision.getName should be("Discount")
    decision.getCategory should be("camunda")
  }

  it should "found deployed decision requirements graph" in {

    val drg = repositoryService
      .createDecisionRequirementsDefinitionQuery()
      .decisionRequirementsDefinitionKey("lending")
      .singleResult()

    drg should not be (null)
    drg.getKey should be("lending")
    drg.getName should be("Lending")
    drg.getCategory should be("camunda")

    val decisions = repositoryService
      .createDecisionDefinitionQuery()
      .decisionRequirementsDefinitionId(drg.getId)
      .list()
      .asScala
      .map(_.getKey)

    decisions should contain("d_RequiredMonthlyInstallment")
  }

  "The decision service'" should "evaluate a decision table with single result" in {

    val result = decisionService
      .evaluateDecisionByKey("discount")
      .variables(Map("customer" -> "Business", "orderSize" -> 7).asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.getSingleEntry[Any] should be(0.1)
  }

  it should "evaluate a decision table with list result" in {

    val result = decisionService
      .evaluateDecisionByKey("holidays")
      .variables(Map("age" -> 58, "yearsOfService" -> 31).asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.collectEntries("Holidays") should be(
      List(22.longValue, 5.longValue, 3.longValue).asJava)
  }

  it should "evaluate a decision table with compound result" in {

    val result = decisionService
      .evaluateDecisionByKey("adjustments")
      .variables(Map("customer" -> "Business", "orderSize" -> 7).asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.getSingleResult.getEntry[Any]("discount") should be(0.1)
    result.getSingleResult.getEntry[Any]("shipping") should be("Air")
  }

  it should "evaluate a decision table with multi-hit compound results" in {

    val result = decisionService
      .evaluateDecisionByKey("routingRules")
      .variables(Map(
        "age" -> 25,
        "riskCategory" -> "MEDIUM",
        "deptReview" -> true).asJava.asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.get(0).getEntry[Any]("routing") should be("REFER")
    result.get(0).getEntry[Any]("reviewLevel") should be("LEVEL 2")
    result.get(0).getEntry[Any]("reason") should be(
      "Applicant under dept review")

    result.get(1).getEntry[Any]("routing") should be("ACCEPT")
    result.get(1).getEntry[Any]("reviewLevel") should be("NONE")
    result.get(1).getEntry[Any]("reason") should be("Acceptable")
  }

  it should "evaluate a decision table no matching rules" in {

    val result = decisionService
      .evaluateDecisionByKey("adjustments")
      .variables(Map("customer" -> "Something else", "orderSize" -> 9).asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result should be(null)
  }

  it should "evaluate an BKM invocation" in {

    val variables = Map(
      "RequestedProduct" -> Map("ProductType" -> "STANDARD LOAN",
                                "Amount" -> 350000,
                                "Rate" -> 0.0395,
                                "Term" -> 360))

    val result = decisionService
      .evaluateDecisionByKey("d_RequiredMonthlyInstallment")
      .variables(variables.asJava.asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.getSingleEntry[Any] should be(1680.8803256086348)
  }

  it should "evaluate a decision literal expression with list result" in {

    val result = decisionService
      .evaluateDecisionByKey("append")
      .variables(Map("list" -> List("a", "b", "c").asJava, "item" -> "d").asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.size should be(1)
    result.getSingleResult.size should be(1)
    result.getSingleEntry[Any] should be(List("a", "b", "c", "d").asJava)
  }

  it should "evaluate a context" in {

    val result = decisionService
      .evaluateDecisionByKey("context")
      .variables(Map("x" -> 1, "y" -> 2).asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.size should be(1)
    result.getSingleResult.size should be(2)
    result.getSingleResult.getEntry[Any]("x") should be(1)
    result.getSingleResult.getEntry[Any]("y") should be(2)
  }

  it should "evaluate a context with aggregation entry" in {

    val result = decisionService
      .evaluateDecisionByKey("contextWithAggregation")
      .variables(Map("x" -> 1, "y" -> 2).asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.size should be(1)
    result.getSingleResult.size should be(1)
    result.getSingleResult.getSingleEntry[Any] should be(3)
  }

  it should "evaluate a relation" in {

    val result = decisionService
      .evaluateDecisionByKey("relation")
      .variables(Map("x" -> "x", "y" -> "y").asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.size should be(2)
    result.get(0).size should be(2)
    result.get(0).getEntry[Any]("x") should be("1: x")
    result.get(0).getEntry[Any]("y") should be("1: y")
    result.get(1).getEntry[Any]("x") should be("2: x")
    result.get(1).getEntry[Any]("y") should be("2: y")
  }

  it should "evaluate a list" in {

    val result = decisionService
      .evaluateDecisionByKey("list")
      .variables(Map("x" -> 1, "y" -> 2).asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.size should be(2)
    result.get(0).size should be(1)
    result.get(0).getSingleEntry[Any] should be(1)
    result.get(1).getSingleEntry[Any] should be(2)
  }

  it should "evaluate a decision by id" in {

    val decisionDefinition = repositoryService
      .createDecisionDefinitionQuery()
      .decisionDefinitionKey("discount")
      .singleResult()

    val result = decisionService
      .evaluateDecisionById(decisionDefinition.getId)
      .variables(Map("customer" -> "Business", "orderSize" -> 7).asJava
        .asInstanceOf[JMap[String, Object]])
      .evaluate()

    result.getSingleEntry[Any] should be(0.1)
  }

  "A business rule task" should "invoke a decision table" in {

    val processInstance = runtimeService.startProcessInstanceByKey(
      "process",
      Map("decisionKey" -> "discount",
          "customer" -> "Business",
          "orderSize" -> 7).asJava.asInstanceOf[JMap[String, Object]])

    runtimeService.getVariable(processInstance.getId, "result") should be(0.1)
  }

  "The history service" should "found historic decision instance for an evaluated decision table" in {

    decisionService
      .evaluateDecisionByKey("routingRules")
      .variables(Map(
        "age" -> 25,
        "riskCategory" -> "MEDIUM",
        "deptReview" -> true).asJava.asInstanceOf[JMap[String, Object]])
      .evaluate()

    val hdi = historyService
      .createHistoricDecisionInstanceQuery()
      .includeInputs()
      .includeOutputs()
      .orderByEvaluationTime()
      .asc()
      .list()
      .asScala
      .last
    hdi.getDecisionDefinitionKey should be("routingRules")

    hdi.getInputs.size should be(3)
    hdi.getInputs.get(0).getClauseName should be("Risk category")
    hdi.getInputs.get(0).getValue should be("MEDIUM")

    hdi.getInputs.get(1).getClauseName should be("Dept review")
    hdi.getInputs.get(1).getValue should be(true: Any)

    hdi.getInputs.get(2).getClauseName should be("Age")
    hdi.getInputs.get(2).getValue should be(25)

    hdi.getOutputs.size should be(6)
    hdi.getOutputs.get(0).getRuleOrder should be(1)
    hdi.getOutputs.get(0).getClauseName should be("Reason")
    hdi.getOutputs.get(0).getValue should be("Acceptable")

    hdi.getOutputs.get(1).getRuleOrder should be(1)
    hdi.getOutputs.get(1).getClauseName should be("Review level")
    hdi.getOutputs.get(1).getValue should be("NONE")

    hdi.getOutputs.get(2).getRuleOrder should be(1)
    hdi.getOutputs.get(2).getClauseName should be("Routing")
    hdi.getOutputs.get(2).getValue should be("ACCEPT")

    hdi.getOutputs.get(3).getRuleOrder should be(2)
    hdi.getOutputs.get(3).getClauseName should be("Reason")
    hdi.getOutputs.get(3).getValue should be("Applicant under dept review")
  }

}
