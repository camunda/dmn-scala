package org.camunda.dmn.evaluation

import org.camunda.dmn.Audit.ContextEvaluationResult
import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.dmn.parser.{ParsedContext, ParsedDecisionLogic}
import org.camunda.feel.context.Context._
import org.camunda.feel.syntaxtree.{Val, ValContext, ValFunction}

class ContextEvaluator(
    eval: (ParsedDecisionLogic, EvalContext) => Either[Failure, Val]) {

  def eval(context: ParsedContext, ctx: EvalContext): Either[Failure, Val] = {

    val auditContext = new AuditContext(Map.empty)

    val result = evalContextEntries(context.entries, ctx).flatMap { results =>
      auditContext.entryResults = results

      evalContextResult(context.aggregationEntry, results, ctx)
    }

    ctx.audit(context,
      result,
      r => ContextEvaluationResult(entries = auditContext.entryResults, result = r))

    result
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

          eval(expr, context)
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

  private class AuditContext(var entryResults: Map[String, Val])

}
