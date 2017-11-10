package org.camunda.dmn

import scala.collection.JavaConverters._

import java.io.InputStream

import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{Decision, DecisionTable, InputEntry, OutputEntry, Output}
import org.camunda.feel.parser.FeelParser
import org.camunda.feel.parser.FeelParser._
import org.camunda.feel.ParsedExpression
import org.camunda.feel.parser.ConstBool

class DmnParser {
  
  def parse(stream: InputStream): Either[ParsedDmn, List[String]] = {
 
    val model = readModel(stream)
    
    val decisions = model.getDefinitions
      .getDrgElements.asScala
      .filter(e => e.isInstanceOf[Decision])
      .map(_.asInstanceOf[Decision])
      
     val decisionTables = decisions.map(d => d.getExpression)
       .filter(_.isInstanceOf[DecisionTable])
       .map(_.asInstanceOf[DecisionTable])
       
     // TODO ensure that single hit policy (U, A, P, F) is only used for decision table with one output
       
     // assume that expression language is FEEL  
     val inputExpressions = decisionTables.flatMap(dt => dt.getInputs.asScala)
       .map(i => {
         val inputExpression = i.getInputExpression
         
         inputExpression.getId -> inputExpression.getText.getTextContent
       })
       .toMap
       
     val outputExpressions = decisionTables.flatMap(dt => dt.getRules.asScala)
       .flatMap(_.getOutputEntries.asScala)
       .map(o => o.getId -> o.getText.getTextContent)
       .toMap
       
     val defaultOutputEntryExpressions = decisionTables.flatMap(dt => dt.getOutputs.asScala)
       .flatMap(o => Option(o.getDefaultOutputEntry))
       .map(o => o.getId -> o.getText.getTextContent)
       .toMap
       
     val parsedUnaryTests = decisionTables.flatMap(dt => dt.getRules.asScala)
       .flatMap(_.getInputEntries.asScala)
       .map(i => {          
         val id = i.getId
         val expression = i.getText.getTextContent
         
         parseUnaryTests(expression) match {
               case Left(exp)    => Left(id -> exp)
               case Right(error) => Right(error)
         }
       })
       .toList
       
     val parsedExpressions = (inputExpressions ++ outputExpressions ++ defaultOutputEntryExpressions)
       .map{ case (id, expression) => parseExpression(expression) match {
               case Left(exp)    => Left(id -> exp)
               case Right(error) => Right(error)
         }
       }
      .toList
     
     val expressions = (parsedExpressions ++ parsedUnaryTests) 
      
     expressions.filter(e => e.isRight) match {
         case Nil     => {
           val e = expressions.map(_.left.get).toMap 
           
           Left(ParsedDmn(model, e))
         }
         case e  => {
           val errors = e.map(_.right.get)
           
           Right(errors)
         }
       }
  }
  
  private def readModel(stream: InputStream): DmnModelInstance = Dmn.readModelFromStream(stream)
  
  private def parseExpression(expression: String): Either[ParsedExpression, String] = {
    FeelParser.parseExpression(expression) match {
      case Success(exp, _) => Left(ParsedExpression(exp, expression))
      case e: NoSuccess    => Right(s"Failed to parse FEEL expression '$expression':\n$e")
    }
  }
  
  private def parseUnaryTests(expression: String): Either[ParsedExpression, String] = {
    
    if (expression.isEmpty()) {
      Left(ParsedExpression(ConstBool(true), expression))
    } else {
      FeelParser.parseUnaryTests(expression) match {
          case Success(exp, _) => Left(ParsedExpression(exp, expression))
          case e: NoSuccess    => Right(s"Failed to parse FEEL unary-tests '$expression':\n$e")
      }
    }
  }
  
}