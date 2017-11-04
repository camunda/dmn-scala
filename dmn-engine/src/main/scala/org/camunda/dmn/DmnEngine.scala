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
      
      evalDecisionTable(expression.asInstanceOf[DecisionTable], context, parsedExpressions) match {
        case None => EvalNull
        case Some(value) => EvalValue(value)
      }
    }
    else {
      EvalFailure(s"decision of type '${expression.getTypeRef}' is not supported")
    }
  }

  private def evalDecisionTable(decisionTable: DecisionTable, context: Map[String,Any], parsedExpressions: Map[String, ParsedExpression]): Option[Any] = {
    
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

    val relevantRules = if (hitPolicy == HitPolicy.FIRST) {
      List(matchedRules.head)
    } else {
      matchedRules
    }  
      
    val outputValues: List[Map[String, Any]] = relevantRules.map(r => {
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
    
    applyHitPolicy(hitPolicy, decisionTable.getAggregation, outputs, outputValues)
  }

  private def applyHitPolicy(
    hitPolicy: HitPolicy,
    aggregator: BuiltinAggregator,
    outputs: List[Output],
    outputValues: List[Map[String, Any]]): Option[Any] = {
    
    hitPolicy match {

      // single hit policies
      case HitPolicy.UNIQUE => {
        // TODO ensure only one result
        singleOutputValue(outputValues)
      }
      case HitPolicy.FIRST => {
        singleOutputValue(outputValues)  
      }
      case HitPolicy.ANY => {
        // TODO ensure output values are equal
        singleOutputValue(outputValues)
      }
      case HitPolicy.PRIORITY => {
        singleOutputValue(sortByPriority(outputValues, outputs))
      }

      // multiple hit policy
      case HitPolicy.OUTPUT_ORDER => {
        multipleOutputValues(sortByPriority(outputValues, outputs))
      }
      case HitPolicy.RULE_ORDER => {
        multipleOutputValues(outputValues)
      }
      case HitPolicy.COLLECT => {
        aggregator match {
          case BuiltinAggregator.COUNT => Some(outputValues.size)
          case BuiltinAggregator.MIN => Some(withSingleOutput(outputValues, withListOfNumbers(_, _.min)))
          case BuiltinAggregator.MAX => Some(withSingleOutput(outputValues, withListOfNumbers(_, _.max)))
          case BuiltinAggregator.SUM => Some(withSingleOutput(outputValues, withListOfNumbers(_, _.sum)))
          case _ => multipleOutputValues(outputValues)
        }
      }

      // default = unique
      case _ => {
        // TODO ensure only one result
        singleOutputValue(outputValues)
      }
    }
  }

  private def withSingleOutput(values: List[Map[String, Any]], f: List[Any] => Any) = f(values.map(_.values.head))
  
  private def withListOfNumbers(values: List[Any], f: List[Double] => Any) = f(values.map(_.asInstanceOf[Double]))
  
  private def sortByPriority(outputValues: List[Map[String, Any]], outputs: List[Output]): List[Map[String, Any]] = {
    val priorities: List[(String, Map[String, Int])] = outputs.map { output =>

      val values = Option(output.getOutputValues).map(
        _.getText.getTextContent
          .split(",")
          .map(_.trim)
          .toList
        ).getOrElse(List())

      output.getName -> values.zipWithIndex.map {
        case (value, index) =>
          value -> index
      }.toMap
    }

    outputValues.sortBy(values =>
      priorities.map {
        case (output, priority) =>
          val value = values(output).toString
          priority.get(value).map(_.toString).getOrElse("")
      }.reduce(_ + _))
  }
  
  private def singleOutputValue(values: List[Map[String, Any]]): Option[Any] = values.headOption.map{v => 
    if (v.size == 1) {
      v.values.head
    } else {
      v
    }
  }

  private def multipleOutputValues(values: List[Map[String, Any]]): Option[Any] = values match {
    case Nil                           => None
    case v :: Nil if (v.size == 1)     => Some(v.values.head)
    case v :: Nil                      => Some(v)
    case list if (list.head.size == 1) => Some(list.map(_.values.head))
    case list => Some(list)
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