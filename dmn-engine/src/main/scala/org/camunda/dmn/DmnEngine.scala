package org.camunda.dmn

import scala.collection.JavaConverters._

import java.io.InputStream

import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{Decision, DecisionTable, InputEntry, OutputEntry, Output}
import org.camunda.feel.FeelEngine
import org.camunda.feel.interpreter.RootContext

class DmnEngine {
  
  lazy val feelEngine = new FeelEngine
  
  def eval(stream: InputStream, decisionId: String, context: Map[String, Any] = Map()): EvalResult = {
    
    val model = readModel(stream)
    
    findDecision(model, decisionId)
      .map(evalDecision(_, context))
      .getOrElse(EvalFailure(s"no decision found with id '$decisionId'"))
  }
  
  private def readModel(stream: InputStream): DmnModelInstance = Dmn.readModelFromStream(stream)
  
  private def findDecision(model: DmnModelInstance, decisionId: String): Option[Decision] = {
    model.getDefinitions
      .getDrgElements.asScala
      .find(e => e.isInstanceOf[Decision] && e.getId == decisionId)
      .map(_.asInstanceOf[Decision])
  }
      
  private def evalDecision(decision: Decision, context: Map[String, Any]): EvalResult = {
    
    val decisionId = decision.getId
    val decisionName = decision.getName
    
    val variable = Option.apply(decision.getVariable)
    val variableName = variable.map(_.getName).getOrElse(decisionId)
    
    val expression = decision.getExpression
    
    if (expression.isInstanceOf[DecisionTable]) {
      
      val result = evalDecisionTable(expression.asInstanceOf[DecisionTable], context)
    
      EvalValue(variableName, result)
    }
    else {
      EvalFailure(s"decision of type '${expression.getTypeRef}' is not supported")
    }
  }

  private def evalDecisionTable(decisionTable: DecisionTable, context: Map[String,Any]): Any = {
    
    val hitPolicy = decisionTable.getHitPolicy
    
    val inputValues = decisionTable.getInputs.asScala
      .map(i => {
        val label = i.getLabel
        val expression = i.getInputExpression.getText.getTextContent
        
        evalExpression(expression, context)
      })
      .toList
   
    val outputs = decisionTable.getOutputs.asScala.toList
      
    val matchedRules = decisionTable.getRules.asScala
      .map(r => {
        val inputEntries = r.getInputEntries.asScala.toList
        
        evalInputEntries(inputEntries, inputValues, context) match {
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
        val expression = o.getText.getTextContent
        
        evalExpression(expression, context) 
      })
      
      outputs.map(_.getName)
        .zip(outputValues)
        .toMap
    })  
    
    // assume that unique hit policy
    val result = outputValues.head
    
    if (result.isEmpty) {
      return null;
    } 
    else if (result.size == 1) {
      return result.head._2  
    }
    else {
     return result 
    } 
  }
  
  private def evalExpression(expression: String, context: Map[String, Any]): Any = {
    feelEngine.evalExpression(expression, context) match {
      case org.camunda.feel.EvalValue(value) => value
      case _ => throw new RuntimeException("todo: handle failure")
    }
  }
  
  private def evalUnaryTests(expression: String, inputValue: Any, context: Map[String, Any]): Boolean = {
    feelEngine.evalUnaryTests(expression, context + (RootContext.defaultInputVariable -> inputValue)) match {
      case org.camunda.feel.EvalValue(value) => value.asInstanceOf[Boolean]
      case _ => throw new RuntimeException("todo: handle failure")
    }
  }
  
  private def evalInputEntries(inputEntries: List[InputEntry], inputValues: List[Any], context: Map[String, Any]): Boolean = {
    inputEntries match {
      case Nil     => true
      case i :: is => {
        val expression = i.getText.getTextContent
        val inputValue = inputValues.head
        
        evalUnaryTests(expression, inputValue, context) match {
          case false => false
          case true => evalInputEntries(is, inputValues.tail, context)
        }
      }
    }
  }
  
}