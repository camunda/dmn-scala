package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.feel.FeelEngine
import org.camunda.bpm.model.dmn.instance.{ Context, ContextEntry, Expression }
import org.camunda.dmn.parser.{ ParsedContext, ParsedDecisionLogic }
import org.camunda.feel.interpreter.{ Val, ValContext, DefaultContext }

class ContextEvaluator(eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val]) {

  def eval(context: ParsedContext, ctx: EvalContext): Either[Failure, Val] = {

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
          .right
          .map(r => ValContext(DefaultContext(r)))
      }
  }

  private def evalContextEntries(entries: Iterable[(String, ParsedDecisionLogic)], ctx: EvalContext): Either[Failure, Map[String, Val]] =
    {
      foldEither[(String, ParsedDecisionLogic), Map[String, Val]](Map[String, Val](), entries, {
        case (result, (name, expr)) =>

          // a context entry must be able to access the result of previous entries
          val context = ctx.copy(variables = ctx.variables ++ result)

          eval(expr, context)
            .right
            .map(v => result + (name -> v))
      })
    }

}
