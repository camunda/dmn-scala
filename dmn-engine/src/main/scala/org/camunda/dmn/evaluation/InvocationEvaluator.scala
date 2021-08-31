package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser.{ParsedBusinessKnowledgeModel, ParsedExpression, ParsedInvocation}
import org.camunda.feel.syntaxtree.Val

class InvocationEvaluator(
    eval: (ParsedExpression, EvalContext) => Either[Failure, Val],
    evalBkm: (ParsedBusinessKnowledgeModel, EvalContext) => Either[Failure, Val]) {

  def eval(invocation: ParsedInvocation,
           context: EvalContext): Either[Failure, Val] = {

    val result = evalParameters(invocation.bindings, context).flatMap { p =>
      val ctx = context.copy(variables = context.variables ++ p.toMap)
      evalBkm(invocation.invocation, ctx)
    }

    context.audit(invocation, result)
    result
  }

  private def evalParameters(
      bindings: Iterable[(String, ParsedExpression)],
      context: EvalContext): Either[Failure, List[(String, Any)]] = {
    mapEither[(String, ParsedExpression), (String, Any)](bindings, {
      case (name, expr) =>
        eval(expr, context)
          .map(name -> _)
    })
  }

}
