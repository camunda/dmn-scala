package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.feel.FeelEngine
import org.camunda.feel.context.Context._
import org.camunda.bpm.model.dmn.instance.{Context, ContextEntry, Expression}
import org.camunda.dmn.parser.{ParsedContext, ParsedDecisionLogic}
import org.camunda.feel.syntaxtree.{Val, ValContext, ValFunction}
import org.camunda.dmn.Audit.ContextEvaluationResult

class ContextEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val]) {

  def eval(context: ParsedContext, ctx: EvalContext): Either[Failure, Val] = {

    evalContextEntries(context.entries, ctx).right.flatMap(results =>
      evalContextResult(context.aggregationEntry, results, ctx).right.map {
        result =>
          ctx.audit(context,
                    ContextEvaluationResult(entries = results, result = result))

          result
    })
  }

  private def evalContextEntries(
      entries: Iterable[(String, ParsedDecisionLogic)],
      ctx: EvalContext): Either[Failure, Map[String, Val]] = {
    foldEither[(String, ParsedDecisionLogic), Map[String, Val]](
      Map[String, Val](),
      entries, {
        case (result, (name, expr)) =>
          // a context entry must be able to access the result of previous entries
          val context = ctx.copy(variables = ctx.variables ++ result)

          eval(expr, context).right
            .map(v => result + (name -> v))
      }
    )
  }

  private def evalContextResult(aggregationEntry: Option[ParsedDecisionLogic],
                                results: Map[String, Val],
                                ctx: EvalContext): Either[Failure, Val] = {

    aggregationEntry
      .map(expr => {
        val evalContext = ctx.copy(variables = ctx.variables ++ results)

        eval(expr, evalContext)
      })
      .getOrElse {
        val functions = results
          .filter { case (k, v) => v.isInstanceOf[ValFunction] }
          .map { case (k, f) => k -> List(f.asInstanceOf[ValFunction]) }

        Right(
          ValContext(StaticContext(variables = results, functions = functions)))
      }
  }

}
