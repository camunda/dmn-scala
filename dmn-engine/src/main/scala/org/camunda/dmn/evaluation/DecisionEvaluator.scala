package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{Decision, Expression}

class DecisionEvaluator(eval: (Expression, EvalContext) => Either[Failure, Any]) {
  
  def eval(decision: Decision, context: EvalContext): Either[Failure, Any] = 
  {
    evalDecision(decision, context)
      .right
      .map{ case (name, result) => result}
  }
  
  private def evalDecision(decision: Decision, context: EvalContext): Either[Failure, (String, Any)] = 
  {
    val decisionId = decision.getId
    val decisionName = Option(decision.getName)

    val variable = Option(decision.getVariable)
    
    val resultName = variable.map(_.getName)
      .orElse(decisionName)
      .getOrElse(decisionId)

    val knowledgeRequirements = decision.getKnowledgeRequirements.asScala
    val informationRequirements = decision.getInformationRequirements.asScala
    
    val requiredDecisions = informationRequirements
      .map(ir => Option(ir.getRequiredDecision))
      .flatten
    
    val requiredDecisionResults = mapEither(requiredDecisions, (d: Decision) => evalDecision(d, context))  
      
    requiredDecisionResults
      .right
      .flatMap(r => 
      {
        val requiredBkms = knowledgeRequirements.map(kr => 
        {
            val bkm = kr.getRequiredKnowledge
            bkm.getName -> bkm
        })
        
        val decisionEvaluationContext = context.copy(
          variables = context.variables ++ r.toMap, 
          bkms = requiredBkms.toMap)
        
        eval(decision.getExpression, decisionEvaluationContext)
          .right
          .map(resultName -> _)
      })
  }
  
}