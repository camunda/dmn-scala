package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.feel.FeelEngine
import org.camunda.bpm.model.dmn.instance.{Context, ContextEntry, Expression}

class ContextEvaluator(eval: (Expression, EvalContext) => Either[Failure, Any]) {
  
  def eval(context: Context, ctx: EvalContext): Either[Failure, Any] = {
  
    val entries = context.getContextEntries.asScala
    val lastEntry = entries.last
    
    val hasFinalResult = Option(lastEntry.getVariable) == None
    
    if (hasFinalResult)
    {
      evalContextEntries(entries.take(entries.size - 1), ctx)
          .right
          .flatMap(results => 
          {
            val context = ctx.copy(variables = ctx.variables ++ results)
          
            eval(lastEntry.getExpression, context)
          })
    }
    else 
    {
      evalContextEntries(entries, ctx)
    }
  }
  
  private def evalContextEntries(entries: Iterable[ContextEntry], ctx: EvalContext): Either[Failure, Map[String, Any]] = 
  {
    foldEither[ContextEntry, Map[String, Any]](Map[String, Any](), entries, { case (result, entry) => 
          
      val varName = entry.getVariable.getName
      // a context entry must be able to access the result of previous entries
      val context = ctx.copy(variables = ctx.variables ++ result)
          
      eval(entry.getExpression, context)
        .right
        .map(v => result + (varName -> v))        
      }) 
  }
  
}