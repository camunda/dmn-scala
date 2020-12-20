package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser.{ParsedBusinessKnowledgeModel, ParsedInvocation}
import org.camunda.feel.syntaxtree.{ParsedExpression, Val}

class InvocationEvaluator(
    eval: (ParsedExpression, EvalContext) => Either[Failure, Val],
    evalBkm: (ParsedBusinessKnowledgeModel, EvalContext) => Either[Failure, Val]) {

  def eval(invocation: ParsedInvocation,
           context: EvalContext): Either[Failure, Val] = {

    evalParameters(invocation.bindings, context).flatMap { p =>
      val ctx = context.copy(variables = context.variables ++ p.toMap)

      val result = evalBkm(invocation.invocation, ctx)
      context.audit(invocation, result)
      result
    }
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
