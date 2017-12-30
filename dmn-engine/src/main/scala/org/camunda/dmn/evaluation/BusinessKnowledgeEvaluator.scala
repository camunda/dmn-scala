package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{BusinessKnowledgeModel, KnowledgeRequirement, FormalParameter, Expression, FunctionDefinition, LiteralExpression}

class BusinessKnowledgeEvaluator(
  eval: (Expression, EvalContext) => Either[Failure, Any],
  evalFunction: (FunctionDefinition, EvalContext) => Either[Failure, Any]) {
  
  def eval(bkm: BusinessKnowledgeModel, context: EvalContext): Either[Failure, (String, Any)] = 
  {
    val name = bkm.getName
    
    val logic = bkm.getEncapsulatedLogic
    val parameters = logic.getFormalParameters.asScala
    val expression = logic.getExpression
    
    val knowledgeRequirements = bkm.getKnowledgeRequirement.asScala
    
    evalRequiredKnowledge(knowledgeRequirements, context).right.flatMap(bkms => 
    {
      expression match {
        case lt: LiteralExpression => 
        {
          evalFunction(logic, context.copy(variables = context.variables ++ bkms))
            .right
            .map(name -> _)
        }
        case _ => Right(name -> createInvocation(expression, parameters, bkms.toMap))
      }
    })
  }
  
  private def evalRequiredKnowledge(knowledgeRequirements: Iterable[KnowledgeRequirement], context: EvalContext): Either[Failure, List[(String, Any)]] = 
  {
    mapEither(knowledgeRequirements, (kr: KnowledgeRequirement) => eval(kr.getRequiredKnowledge, context))
  }
  
  private def createInvocation(expression: Expression, parameters: Iterable[FormalParameter], bkms: Map[String, Any]): BkmInvocation = 
  {
    BkmInvocation(ctx => 
    {
      validateParameters(parameters, ctx)
        .right
        .flatMap(_ => eval(expression, ctx.copy(variables = ctx.variables ++ bkms)))
    })
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