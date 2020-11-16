package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._
import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{Binding, Invocation, Parameter}
import org.camunda.bpm.model.dmn.instance.{BusinessKnowledgeModel, Expression, LiteralExpression}
import org.camunda.dmn.parser.{ParsedBusinessKnowledgeModel, ParsedDecisionLogic, ParsedInvocation}
import org.camunda.feel.syntaxtree.{ParsedExpression, Val, ValError}
import org.camunda.dmn.Audit.SingleEvaluationResult

class InvocationEvaluator(
    eval: (ParsedExpression, EvalContext) => Either[Failure, Val],
    evalBkm: (ParsedBusinessKnowledgeModel, EvalContext) => Either[Failure, Val]) {

  def eval(invocation: ParsedInvocation,
           context: EvalContext): Either[Failure, Val] = {

    evalParameters(invocation.bindings, context).flatMap { p =>
      val ctx = context.copy(variables = context.variables ++ p.toMap)

      evalBkm(invocation.invocation, ctx) match {
        case r@Right(result) =>
          context.audit(invocation, SingleEvaluationResult(result))
          r
        case l@Left(failure) =>
          context.audit(invocation, SingleEvaluationResult(ValError(failure.message)))
          l
      }
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
