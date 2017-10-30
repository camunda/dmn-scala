package org.camunda.dmn

import scala.collection.JavaConverters._

import java.io.InputStream

import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{Decision, DecisionTable, InputEntry, OutputEntry, Output}
import org.camunda.feel.FeelEngine
import org.camunda.feel.interpreter.RootContext
import org.camunda.feel.ParsedExpression

class DmnEngine {
  
  lazy val feelEngine = new FeelEngine
  
  val parser = new DmnParser
  
  def eval(stream: InputStream, decisionId: String, context: Map[String, Any]): EvalResult = {
    parse(stream) match {
      case Right(errors)    => ParseFailure(s"Failed to parse DMN: ${errors.mkString("\n")}")
      case Left(parsedDmn)  => eval(parsedDmn, decisionId, context)
    }
  }
  
  def parse(stream: InputStream): Either[ParsedDmn, List[String]] = parser.parse(stream)
  
  def eval(dmn: ParsedDmn, decisionId: String, context: Map[String, Any]): EvalResult = {
    findDecision(dmn.model, decisionId)
          .map(evalDecision(_, context, dmn.expressionsById))
          .getOrElse(EvalFailure(s"no decision found with id '$decisionId'"))
  }
  
  private def findDecision(model: DmnModelInstance, decisionId: String): Option[Decision] = {
    model.getDefinitions
      .getDrgElements.asScala
      .find(e => e.isInstanceOf[Decision] && e.getId == decisionId)
      .map(_.asInstanceOf[Decision])
  }
      
  private def evalDecision(decision: Decision, context: Map[String, Any], parsedExpressions: Map[String, ParsedExpression]): EvalResult = {
    
    val decisionId = decision.getId
    val decisionName = decision.getName
    
    val variable = Option.apply(decision.getVariable)
    val variableName = variable.map(_.getName).getOrElse(decisionId)
    
    val expression = decision.getExpression
    
    if (expression.isInstanceOf[DecisionTable]) {
      
      val result = evalDecisionTable(expression.asInstanceOf[DecisionTable], context, parsedExpressions)
    
      if (result.isEmpty) {
        EvalNull;
      } 
      else if (result.size == 1) {
        EvalValue(result.head._2)
      }
      else {
        EvalValue(result) 
      } 
      
    }
    else {
      EvalFailure(s"decision of type '${expression.getTypeRef}' is not supported")
    }
  }

  private def evalDecisionTable(decisionTable: DecisionTable, context: Map[String,Any], parsedExpressions: Map[String, ParsedExpression]): Map[String, Any] = {
    
    val hitPolicy = decisionTable.getHitPolicy
    
    val inputValues = decisionTable.getInputs.asScala
      .map(i => {
        val id = i.getInputExpression.getId
        val expression = parsedExpressions(id)
        
        evalExpression(expression, context)
      })
      .toList
   
    val outputs = decisionTable.getOutputs.asScala.toList
      
    val matchedRules = decisionTable.getRules.asScala
      .map(r => {
        val inputEntries = r.getInputEntries.asScala.toList
        
        evalInputEntries(inputEntries, inputValues, context, parsedExpressions) match {
          case false  => None
          case true   => Some(r)
        }
      })
      .flatten
      .toList
    
    // TODO check hit policy  
      
    val outputValues = matchedRules.map(r => {
      val outputEntries = r.getOutputEntries.asScala.toList
       
      val outputValues = outputEntries.map(o => {
        val id = o.getId
        val expression = parsedExpressions(id)
        
        evalExpression(expression, context) 
      })
      
      outputs.map(_.getName)
        .zip(outputValues)
        .toMap
    })  
    
    // assume that unique hit policy
    outputValues.headOption.getOrElse(Map.empty)
  }
  
  private def evalExpression(expression: ParsedExpression, context: Map[String, Any]): Any = {
    feelEngine.eval(expression, context) match {
      case org.camunda.feel.EvalValue(value) => value
      case _ => throw new RuntimeException("todo: handle failure")
    }
  }
  
  private def evalInputEntries(inputEntries: List[InputEntry], inputValues: List[Any], context: Map[String, Any], parsedExpressions: Map[String, ParsedExpression]): Boolean = {
    inputEntries match {
      case Nil     => true
      case i :: is => {
        val id = i.getId
        val expression = parsedExpressions(id)
        
        val inputValue = inputValues.head
        val contextWithInput = context + (RootContext.defaultInputVariable -> inputValue)
        
        evalExpression(expression, contextWithInput) match {
          case false => false
          case true => evalInputEntries(is, inputValues.tail, context, parsedExpressions)
        }
      }
    }
  }
  
}