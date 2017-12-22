package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{BusinessKnowledgeModel, FormalParameter, Expression}

class BusinessKnowledgeEvaluator(eval: (Expression, EvalContext) => Either[Failure, Any]) {
  
  def eval(bkm: BusinessKnowledgeModel, context: EvalContext): Either[Failure, Any] = 
  {
    val logic = bkm.getEncapsulatedLogic
    val expression = logic.getExpression
    
    val knowledgeRequirements = bkm.getKnowledgeRequirement.asScala

    val requiredBkms = knowledgeRequirements.map(kr => {
          val bkm = kr.getRequiredKnowledge
          bkm.getName -> bkm
        })
        .toMap
                
    val contextWithImports = context.copy(bkms = requiredBkms)
        
    val parameters = logic.getFormalParameters.asScala

    validateParameters(parameters, context)
      .right
      .flatMap(_ => eval(expression, contextWithImports))
  }
  
  private def validateParameters(parameters: Iterable[FormalParameter], context: EvalContext): Either[Failure, List[Any]] = 
  {
    // TODO check type of parameters
    mapEither(parameters, (p: FormalParameter) => 
      context.variables.get(p.getName)
        .map(Right(_))
        .getOrElse(Left(Failure(s"no parameter found with name '${p.getName}'")))
    )  
  }
  
}