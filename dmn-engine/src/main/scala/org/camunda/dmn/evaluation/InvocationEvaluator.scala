package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{Invocation, Parameter, Binding}
import org.camunda.bpm.model.dmn.instance.{
  LiteralExpression,
  BusinessKnowledgeModel,
  Expression
}
import org.camunda.dmn.parser.{
  ParsedDecisionLogic,
  ParsedInvocation,
  ParsedBusinessKnowledgeModel
}
import org.camunda.feel.ParsedExpression
import org.camunda.feel.interpreter.Val
import org.camunda.dmn.Audit.SingleEvaluationResult

class InvocationEvaluator(
    eval: (ParsedExpression, EvalContext) => Either[Failure, Val],
    evalBkm: (ParsedBusinessKnowledgeModel, EvalContext) => Either[Failure, Val]) {

  def eval(invocation: ParsedInvocation,
           context: EvalContext): Either[Failure, Val] = {

    evalParameters(invocation.bindings, context).right.flatMap { p =>
      val ctx = context.copy(variables = context.variables ++ p.toMap)

      evalBkm(invocation.invocation, ctx).right
        .map { result =>
          context.audit(invocation, SingleEvaluationResult(result))

          result
        }
    }
  }

  private def evalParameters(
      bindings: Iterable[(String, ParsedExpression)],
      context: EvalContext): Either[Failure, List[(String, Any)]] = {
    mapEither[(String, ParsedExpression), (String, Any)](bindings, {
      case (name, expr) =>
        eval(expr, context).right
          .map(name -> _)
    })
  }

}
