package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{Decision, Expression}

class DecisionEvaluator(eval: (Expression, EvalContext) => Either[Failure, Any]) {
  
  def eval(decision: Decision, context: EvalContext): Either[Failure, Any] = {

    val decisionId = decision.getId
    val decisionName = decision.getName

    val variable = Option.apply(decision.getVariable)
    val variableName = variable.map(_.getName).getOrElse(decisionId)

    val knowledgeRequirements = decision.getKnowledgeRequirements.asScala
    
    // TODO evaluate required decision
    
    val requiredBkms = knowledgeRequirements.map(kr => {
      val bkm = kr.getRequiredKnowledge
      bkm.getName -> bkm
    })
    .toMap
            
    val contextWithImports = context.copy(bkms = requiredBkms)

    eval(decision.getExpression, contextWithImports)
  }
  
}