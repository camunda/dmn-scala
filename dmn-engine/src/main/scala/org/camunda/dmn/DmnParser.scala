package org.camunda.dmn

import scala.collection.JavaConverters._

import java.io.InputStream

import org.camunda.dmn.DmnEngine.Failure
import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{Decision, BusinessKnowledgeModel, Invocation, LiteralExpression}
import org.camunda.bpm.model.dmn.instance.{Decision, DecisionTable, InputEntry, OutputEntry, Output}
import org.camunda.bpm.model.dmn.instance.{Context, ContextEntry}
import org.camunda.feel.parser.FeelParser
import org.camunda.feel.parser.FeelParser.{Success, NoSuccess}
import org.camunda.feel.ParsedExpression
import org.camunda.feel.parser.ConstBool

class DmnParser {

  type ParseResult = Iterable[Either[Failure, (String, ParsedExpression)]]
  
  case class ParsingContext(parsedExpressions: Map[String, ParsedExpression] = Map.empty)
  
  def parse(stream: InputStream): Either[Failure, ParsedDmn] = {
 
    val model = readModel(stream)
    
    val drgElements = model.getDefinitions.getDrgElements.asScala
    
    val result: ParseResult = (List[Either[Failure, (String, ParsedExpression)]]() /: drgElements){ case (result, element) => {
      
      val parsedExpressions = result
        .filter(_.isRight)
        .map(_.right.get)
        .toMap
      
      val ctx = ParsingContext(parsedExpressions)
      
      val p = element match {
        case d: Decision                 => parseDecision(d)(ctx)
        case bkm: BusinessKnowledgeModel => parseBusinessKnowledgeModel(bkm)(ctx)
        case other                       => List.empty // ignore 
      }
      
      result ++ p
    }}
      
    result.filter(e => e.isLeft) match {
         case Nil     => {
           val e = result.map(_.right.get).toMap 
           
           Right(ParsedDmn(model, e))
         }
         case e  => {
           val errors = e.map(_.left.get)
           
           Left(Failure(errors.mkString("\n")))
         }
       }
  }  
  
  private def readModel(stream: InputStream): DmnModelInstance = Dmn.readModelFromStream(stream)
  
  private def parseDecision(decision: Decision)(implicit ctx: ParsingContext): ParseResult = {
    
    decision.getExpression match {
      case dt: DecisionTable => parseDecisionTable(dt)
      case inv: Invocation   => parseInvocation(inv)
      case c: Context        => parseContext(c)
      case other             => List(Left(Failure(s"unsupported decision expression '$other'")))
    }
  }
  
  private def parseDecisionTable(decisionTable: DecisionTable)(implicit ctx: ParsingContext): ParseResult = {
    
    // TODO ensure that single hit policy (U, A, P, F) is only used for decision table with one output
    
    val rules = decisionTable.getRules.asScala
    
    val inputExpressions = decisionTable.getInputs.asScala
       .map(_.getInputExpression)
       .map(_.getText.getTextContent)
       
     val defaultOutputEntryExpressions = decisionTable.getOutputs.asScala
       .flatMap(o => Option(o.getDefaultOutputEntry))
       .map(_.getText.getTextContent)
       
     val outputExpressions = rules
       .flatMap(_.getOutputEntries.asScala)
       .map(_.getText.getTextContent)
       
     val parsedExpressions = (inputExpressions ++ outputExpressions ++ defaultOutputEntryExpressions)
       .toList
       .distinct
       .filter(!ctx.parsedExpressions.contains(_))
       .map(expr => parseExpression(expr).right.map(expr -> _))
       
     val unaryTests = rules
       .flatMap(_.getInputEntries.asScala)
       .map(_.getText.getTextContent)
    
    val parsedUnaryTests = unaryTests
      .filter(!ctx.parsedExpressions.contains(_))
      .map(expr => parseUnaryTests(expr).right.map(expr -> _))
    
      
    parsedExpressions ++ parsedUnaryTests
  }
  
  private def parseInvocation(invocation: Invocation)(implicit ctx: ParsingContext): ParseResult = {
    
    val bindings = invocation.getBindings.asScala
        
    val expressions = bindings.map(_.getExpression match {
      case lt: LiteralExpression => Right(lt.getText.getTextContent)
      case other => Left(Failure(s"expected binding with literal expression but found '$other'"))
    })
    
    val failures = expressions
      .filter(_.isLeft)
      .map(f => Left(f.left.get))

    val literalExpressions = expressions
      .filter(_.isRight)
      .map(_.right.get)
        
    val parsedExpressions = literalExpressions
      .filter(!ctx.parsedExpressions.contains(_))
      .map(expr => parseExpression(expr).right.map(expr -> _))
      
    parsedExpressions ++ failures
  }
  
  private def parseContext(context: Context)(implicit ctx: ParsingContext): ParseResult = {
    
    val entries = context.getContextEntries.asScala
    val expressions = entries.map(_.getExpression)
   
    val literalExpressions = expressions
      .filter(_.isInstanceOf[LiteralExpression])
      .map(_.asInstanceOf[LiteralExpression])
      .map(_.getText.getTextContent)
      
    val parsedExpressions = literalExpressions
      .filter(!ctx.parsedExpressions.contains(_))
      .map(expr => parseExpression(expr).right.map(expr -> _))  
      
    // TODO parse also other expressions (decision table, nested context, invocation=
    
    parsedExpressions
  }
  
  private def parseBusinessKnowledgeModel(bkm: BusinessKnowledgeModel)(implicit ctx: ParsingContext): ParseResult = {
    
    val logic = bkm.getEncapsulatedLogic
    
    logic.getExpression match {
      case dt: DecisionTable => parseDecisionTable(dt)
      case other => List(Left(Failure(s"expected BKM with decision table but found '$other'")))
    }
  }
  
  private def parseExpression(expression: String): Either[Failure, ParsedExpression] = {
    FeelParser.parseExpression(expression) match {
      case Success(exp, _) => Right(ParsedExpression(exp, expression))
      case e: NoSuccess    => Left(Failure(s"Failed to parse FEEL expression '$expression':\n$e"))
    }
  }
  
  private def parseUnaryTests(expression: String): Either[Failure, ParsedExpression] = {
    
    if (expression.isEmpty()) {
      Right(ParsedExpression(ConstBool(true), expression))
    } else {
      FeelParser.parseUnaryTests(expression) match {
          case Success(exp, _) => Right(ParsedExpression(exp, expression))
          case e: NoSuccess    => Left(Failure(s"Failed to parse FEEL unary-tests '$expression':\n$e"))
      }
    }
  }
  
}