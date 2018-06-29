package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.feel.FeelEngine
import org.camunda.bpm.model.dmn.instance.{ Context, ContextEntry, Expression }
import org.camunda.dmn.parser.ParsedContext
import org.camunda.dmn.parser.ParsedDecisionLogic
import org.camunda.dmn.parser.ParsedDecisionLogic
import org.camunda.dmn.parser.ParsedDecisionLogic

class ContextEvaluator(eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Any]) {

  def eval(context: ParsedContext, ctx: EvalContext): Either[Failure, Any] = {

    context.aggregationEntry
      .map(expr => {
        evalContextEntries(context.entries, ctx)
          .right
          .flatMap(results => {
            val context = ctx.copy(variables = ctx.variables ++ results)

            eval(expr, context)
          })
      }).getOrElse {
        evalContextEntries(context.entries, ctx)
      }
  }

  private def evalContextEntries(entries: Iterable[(String, ParsedDecisionLogic)], ctx: EvalContext): Either[Failure, Map[String, Any]] =
    {
      foldEither[(String, ParsedDecisionLogic), Map[String, Any]](Map[String, Any](), entries, {
        case (result, (name, expr)) =>

          // a context entry must be able to access the result of previous entries
          val context = ctx.copy(variables = ctx.variables ++ result)

          eval(expr, context)
            .right
            .map(v => result + (name -> v))
      })
    }

}
