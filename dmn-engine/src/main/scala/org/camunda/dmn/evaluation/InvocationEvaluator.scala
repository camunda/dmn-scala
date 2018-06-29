package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.bpm.model.dmn.instance.{ Invocation, Parameter, Binding }
import org.camunda.bpm.model.dmn.instance.{ LiteralExpression, BusinessKnowledgeModel, Expression }
import org.camunda.dmn.parser.ParsedDecisionLogic
import org.camunda.dmn.parser.ParsedInvocation
import org.camunda.feel.ParsedExpression
import org.camunda.dmn.parser.ParsedBusinessKnowledgeModel

class InvocationEvaluator(
  eval:    (ParsedExpression, EvalContext) => Either[Failure, Any],
  evalBkm: (ParsedBusinessKnowledgeModel, EvalContext) => Either[Failure, Any]) {

  def eval(invocation: ParsedInvocation, context: EvalContext): Either[Failure, Any] = {
    evalParameters(invocation.bindings, context).right.flatMap(p => {
      val ctx = context.copy(variables = context.variables ++ p.toMap)

      evalBkm(invocation.invocation, ctx)
    })
  }

  private def evalParameters(bindings: Iterable[(String, ParsedExpression)], context: EvalContext): Either[Failure, List[(String, Any)]] = {
    mapEither[(String, ParsedExpression), (String, Any)](bindings, {
      case (name, expr) => eval(expr, context)
        .right
        .map(name -> _)
    })
  }

}
