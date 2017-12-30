package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{Invocation, Parameter, Binding}
import org.camunda.bpm.model.dmn.instance.{LiteralExpression, BusinessKnowledgeModel, Expression}

class InvocationEvaluator(eval: (Expression, EvalContext) => Either[Failure, Any]) {

  def eval(invocation: Invocation, context: EvalContext): Either[Failure, Any] = 
  {
    evalInvocationExpression(invocation.getExpression, context).right.flatMap(bkm => 
    {
      val bindings = invocation.getBindings.asScala

      evalParameters(bindings, context).right.flatMap(p => 
      {
        val ctx = context.copy(variables = context.variables ++ p.toMap)

        bkm.invoke(ctx)
      })
    })
  }
  
  private def evalInvocationExpression(expr: Expression, context: EvalContext): Either[Failure, BkmInvocation] = 
  {
    expr match {
      case le: LiteralExpression =>
      {
          val bkmName = le.getText.getTextContent
                  
          context.variables.get(bkmName)
            .filter(_.isInstanceOf[BkmInvocation])
            .map(bkm => Right(bkm.asInstanceOf[BkmInvocation]))
            .getOrElse(Left(Failure(s"no BKM found with name '$bkmName'")))
      }
      case other => Left(Failure(s"expected invocation with literal expression but found '$other'"))
    }
  }
  
  private def evalParameters(bindings: Iterable[Binding], context: EvalContext): Either[Failure, List[(String, Any)]] = 
  {
    mapEither(bindings, (binding: Binding) => 
    {
      val paramName = binding.getParameter.getName
      
      eval(binding.getExpression, context)
        .right
        .map(value => paramName -> value)
    })
  }
  
}
