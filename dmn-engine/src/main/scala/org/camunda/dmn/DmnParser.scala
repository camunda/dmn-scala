package org.camunda.dmn

import scala.collection.JavaConverters._

import java.io.InputStream

import org.camunda.dmn.DmnEngine.Failure
import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{Decision, DecisionTable, InputEntry, OutputEntry, Output, BusinessKnowledgeModel, Invocation, LiteralExpression}
import org.camunda.feel.parser.FeelParser
import org.camunda.feel.parser.FeelParser.{Success, NoSuccess}
import org.camunda.feel.ParsedExpression
import org.camunda.feel.parser.ConstBool

class DmnParser {

  case class ParseFailure(error: String)
  
  def parse(stream: InputStream): Either[Failure, ParsedDmn] = {
 
    val model = readModel(stream)
    
    val drgElements = model.getDefinitions.getDrgElements.asScala
    
    val decisions = drgElements
      .filter(_.isInstanceOf[Decision])
      .map(_.asInstanceOf[Decision])
      
     val bkms = drgElements
       .filter(_.isInstanceOf[BusinessKnowledgeModel])
       .map(_.asInstanceOf[BusinessKnowledgeModel])
       
     val drgExpressions = 
       decisions.map(_.getExpression) ++ 
       bkms.map(_.getEncapsulatedLogic).map(_.getExpression)  
      
     val literalExpressions = drgExpressions
       .filter(_.isInstanceOf[Invocation])
       .map(_.asInstanceOf[Invocation])
       .flatMap(_.getBindings.asScala)
       .map(_.getExpression.asInstanceOf[LiteralExpression])
       .map(e => e.getId -> e.getText.getTextContent)
       .toMap
       
     val decisionTables = drgExpressions
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
         
         parseUnaryTests(expression).right.map(id -> _)
       })
       .toList
       
     val parsedExpressions = (inputExpressions ++ outputExpressions ++ defaultOutputEntryExpressions ++ literalExpressions)
       .map{ case (id, expression) => parseExpression(expression).right.map(id -> _)
       }
      .toList
     
     val expressions = (parsedExpressions ++ parsedUnaryTests) 
      
     expressions.filter(e => e.isLeft) match {
         case Nil     => {
           val e = expressions.map(_.right.get).toMap 
           
           Right(ParsedDmn(model, e))
         }
         case e  => {
           val errors = e.map(_.left.get)
           
           Left(Failure(errors.mkString("\n")))
         }
       }
  }
  
  private def readModel(stream: InputStream): DmnModelInstance = Dmn.readModelFromStream(stream)
  
  private def parseExpression(expression: String): Either[ParseFailure, ParsedExpression] = {
    FeelParser.parseExpression(expression) match {
      case Success(exp, _) => Right(ParsedExpression(exp, expression))
      case e: NoSuccess    => Left(ParseFailure(s"Failed to parse FEEL expression '$expression':\n$e"))
    }
  }
  
  private def parseUnaryTests(expression: String): Either[ParseFailure, ParsedExpression] = {
    
    if (expression.isEmpty()) {
      Right(ParsedExpression(ConstBool(true), expression))
    } else {
      FeelParser.parseUnaryTests(expression) match {
          case Success(exp, _) => Right(ParsedExpression(exp, expression))
          case e: NoSuccess    => Left(ParseFailure(s"Failed to parse FEEL unary-tests '$expression':\n$e"))
      }
    }
  }
  
}