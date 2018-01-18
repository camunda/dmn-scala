package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.feel.interpreter.{ValFunction, ValError, DefaultValueMapper}
import org.camunda.bpm.model.dmn.instance.{BusinessKnowledgeModel, KnowledgeRequirement, FormalParameter, Expression, LiteralExpression}

class BusinessKnowledgeEvaluator(eval: (Expression, EvalContext) => Either[Failure, Any]) {
  
  def eval(bkm: BusinessKnowledgeModel, context: EvalContext): Either[Failure, (String, (BkmInvocation, ValFunction))] = 
  {
    val name = bkm.getName
    
    val logic = bkm.getEncapsulatedLogic
    val parameters = logic.getFormalParameters.asScala
    val expression = logic.getExpression
    
    val knowledgeRequirements = bkm.getKnowledgeRequirement.asScala
    
    evalRequiredKnowledge(knowledgeRequirements, context).right.map(bkms => 
    {
      	val invocations = bkms.map{ case (key, (inv, f)) => key -> inv}
        val functions = bkms.map{ case (key, (inv, f)) => key -> f}
      
        val evalContext = context.copy(
            variables = context.variables ++ functions, 
            bkms = context.bkms ++ invocations)
      
        val invocation = createInvocation(expression, parameters, evalContext)  
        val function = createFunction(expression, parameters, evalContext)
      
        name -> (invocation, function)
    })
  }
  
  private def evalRequiredKnowledge(knowledgeRequirements: Iterable[KnowledgeRequirement], context: EvalContext): Either[Failure, List[(String, (BkmInvocation, ValFunction))]] = 
  {
    mapEither(knowledgeRequirements, (kr: KnowledgeRequirement) => eval(kr.getRequiredKnowledge, context))
  }
  
  private def createInvocation(expression: Expression, parameters: Iterable[FormalParameter], context: EvalContext): BkmInvocation = 
  {
    BkmInvocation(ctx => 
    {
      val evalContext = ctx.copy(
          variables = ctx.variables ++ context.variables, 
          bkms = ctx.bkms ++ context.bkms)
      
      validateParameters(parameters, ctx)
        .right
        .flatMap(_ => eval(expression, evalContext))
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
  
  private def createFunction(expression: Expression, parameters: Iterable[FormalParameter], context: EvalContext): ValFunction = 
  {
    val parameterNames = parameters.map(_.getName).toList
    
    ValFunction(
      params = parameterNames,
      invoke = args => 
        {
          // TODO check type of parameters          
          val arguments = parameterNames.zip(args)
          
          eval(expression, context.copy(variables = context.variables ++ arguments)) match {
            case Right(value) => DefaultValueMapper.instance.toVal(value)
            case Left(error)  => ValError(error.toString)
          }
        } 
    )
  }
  
}