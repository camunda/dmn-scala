package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.feel.interpreter.ValFunction
import org.camunda.bpm.model.dmn.instance.{Decision, Expression, BusinessKnowledgeModel, KnowledgeRequirement, InformationRequirement}

class DecisionEvaluator(
  eval: (Expression, EvalContext) => Either[Failure, Any], 
  evalBkm: (BusinessKnowledgeModel, EvalContext) => Either[Failure, (String, (BkmInvocation, ValFunction))]) {
  
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
    
    val resultType = variable.flatMap(v => Option(v.getTypeRef))
    
    val resultName = variable.map(_.getName)
      .orElse(decisionName)
      .getOrElse(decisionId)

    val knowledgeRequirements = decision.getKnowledgeRequirements.asScala
    val informationRequirements = decision.getInformationRequirements.asScala
      
    evalRequiredDecisions(informationRequirements, context).right.flatMap(decisionResults => 
    {
      evalRequiredKnowledge(knowledgeRequirements, context).right.flatMap(bkms => 
      {
        val invocations = bkms.map{ case (key, (inv, f)) => key -> inv}
        val functions = bkms.map{ case (key, (inv, f)) => key -> f}
        
        val decisionEvaluationContext = context.copy(
            variables = context.variables ++ decisionResults ++ functions,
            bkms = context.bkms ++ invocations)
        
        eval(decision.getExpression, decisionEvaluationContext)
          .right
          .flatMap(r =>
            resultType
              .map(t => TypeChecker.isOfType(r, t, context))
              .getOrElse(Right(r))  
          )
          .right
          .map(resultName -> _)            
      })
    })
  }
  
  private def evalRequiredDecisions(informationRequirements: Iterable[InformationRequirement], context: EvalContext): Either[Failure, List[(String, Any)]] = 
  {
    val requiredDecisions = informationRequirements
      .map(ir => Option(ir.getRequiredDecision))
      .flatten
    
    mapEither(requiredDecisions, (d: Decision) => evalDecision(d, context))  
  }
  
  private def evalRequiredKnowledge(knowledgeRequirements: Iterable[KnowledgeRequirement], context: EvalContext): Either[Failure, List[(String, (BkmInvocation, ValFunction))]] = 
  {
    mapEither(knowledgeRequirements, (kr: KnowledgeRequirement) => evalBkm(kr.getRequiredKnowledge, context))
  }
  
}