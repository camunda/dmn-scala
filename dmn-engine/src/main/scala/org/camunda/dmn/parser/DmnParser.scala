package org.camunda.dmn.parser

import scala.collection.JavaConverters._

import java.io.InputStream

import org.camunda.dmn.DmnEngine.Failure
import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{Decision, BusinessKnowledgeModel, Invocation}
import org.camunda.bpm.model.dmn.instance.{DecisionTable, InputEntry, OutputEntry, Output}
import org.camunda.bpm.model.dmn.instance.{LiteralExpression, Expression}
import org.camunda.bpm.model.dmn.instance.{Context, ContextEntry}
import org.camunda.bpm.model.dmn.instance.{List => DmnList, Relation, FunctionDefinition}
import org.camunda.feel.parser.FeelParser
import org.camunda.feel.parser.FeelParser.{Success, NoSuccess}
import org.camunda.feel.ParsedExpression
import org.camunda.feel.parser.ConstBool

class DmnParser {

  object ExpressionType extends Enumeration {
    type ExpressionType = Value
    val FeelExpression, UnaryTests = Value
  }
  
  import ExpressionType._
  
  type ParsedExpressionTuple = (String, (ParsedExpression, ExpressionType))
  
  type ParseResult = Iterable[Either[Failure, ParsedExpressionTuple]]
  
  case class ParsingContext(
      model: DmnModelInstance,
      parsedExpressions: Map[String, ParsedExpression] = Map.empty,
      parsedUnaryTest: Map[String, ParsedExpression] = Map.empty
  )
  
  def parse(stream: InputStream): Either[Failure, ParsedDmn] = 
  {
    try
    {
    
      val model = readModel(stream)
      
      val drgElements = model.getDefinitions.getDrgElements.asScala
            
      val result: ParseResult = (List[Either[Failure, ParsedExpressionTuple]]() /: drgElements){ case (result, element) => {
        
        val parsedExpressions = result
          .filter(_.isRight)
          .map(_.right.get)
          .toMap
          
        val ctx = ParsingContext(model, getFeelExpressions(parsedExpressions), getUnaryTests(parsedExpressions))
        
        val p = element match {
          case d: Decision                 => parseDecision(d)(ctx)
          case bkm: BusinessKnowledgeModel => parseBusinessKnowledgeModel(bkm)(ctx)
          case other                       => List.empty // ignore 
        }
        
        result ++ p
      }}
        
      result.filter(e => e.isLeft) match {
           case Nil     => {
             val e = result.map(_.right.get)
             
             Right(ParsedDmn(model, getFeelExpressions(e), getUnaryTests(e)))
           }
           case e  => {
             val errors = e.map(_.left.get)
             
             Left(Failure(errors.mkString("\n")))
           }
         }
    } catch {
      case t: Throwable => Left(Failure(s"Failed to parse DMN: $t"))
    }
  }  
  
  private def getFeelExpressions(expressions: Iterable[ParsedExpressionTuple]): Map[String, ParsedExpression] = 
  {
    expressions
      .filter{ case (expr, (p, t)) => t == FeelExpression }
      .map{ case (expr, (p, t)) => expr -> p }
      .toMap
  }
  
  private def getUnaryTests(expressions: Iterable[ParsedExpressionTuple]): Map[String, ParsedExpression] = 
  {
    expressions
      .filter{ case (expr, (p, t)) => t == UnaryTests }
      .map{ case (expr, (p, t)) => expr -> p }
      .toMap
  }
  
  private def readModel(stream: InputStream): DmnModelInstance = Dmn.readModelFromStream(stream)
  
  private def parseDecision(decision: Decision)(implicit ctx: ParsingContext): ParseResult = {
    
    decision.getExpression match {
      case dt: DecisionTable     => parseDecisionTable(dt)
      case inv: Invocation       => parseInvocation(inv)
      case c: Context            => parseContext(c)
      case r: Relation           => parseRelation(r)
      case lt: LiteralExpression => parseLiteralExpression(lt)
      case other                 => List(Left(Failure(s"unsupported decision expression '$other'")))
    }
  }
   
  private def parseBusinessKnowledgeModel(bkm: BusinessKnowledgeModel)(implicit ctx: ParsingContext): ParseResult = {
    
    val logic = bkm.getEncapsulatedLogic
    
    logic.getExpression match {
      case dt: DecisionTable     => parseDecisionTable(dt)
      case c: Context            => parseContext(c)
      case rel: Relation         => parseRelation(rel)(ctx)
      case lt: LiteralExpression => parseLiteralExpression(lt)(ctx)
      case other => List(Left(Failure(s"unsupported business knowledge model logic found '$other'")))
    }
  }
  
  private def parseDecisionTable(decisionTable: DecisionTable)(implicit ctx: ParsingContext): ParseResult = {
    
    val failures = if (decisionTable.getOutputs.size > 1 && 
        decisionTable.getHitPolicy.equals(HitPolicy.COLLECT) && 
        Option(decisionTable.getAggregation).isEmpty) 
    {
      List(Left(Failure("hit policy 'COLLECT' with aggregator is not defined for compound output")))
    } else {
      List()
    }
    
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
      .toList
      .distinct
      .filter(!ctx.parsedUnaryTest.contains(_))
      .map(expr => parseUnaryTests(expr).right.map(expr -> _))
      
    parsedExpressions ++ parsedUnaryTests ++ failures
  }
  
  private def parseLiteralExpression(expression: LiteralExpression)(implicit ctx: ParsingContext): ParseResult = 
  {
    val expr = expression.getText.getTextContent
    
    if (ctx.parsedExpressions.contains(expr)) 
    {
      List.empty
    }
    else
    {
      List( parseExpression(expr).right.map(expr -> _))
    }
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
      
    val invocationFailures = invocation.getExpression match {
      case lt: LiteralExpression => 
      {
        val expression = lt.getText.getTextContent  
        
        val bkmNames = ctx.model.getDefinitions.getDrgElements.asScala
          .filter(_.isInstanceOf[BusinessKnowledgeModel])
          .map(_.asInstanceOf[BusinessKnowledgeModel].getName)
          .toList
        
        if (!bkmNames.contains(expression)) {
          List(Left(Failure(s"no BKM found with name '$expression'")))
        } else {
          List()
        } 
      }
      case other => List(Left(Failure(s"expected invocation with literal expression but found '$other'")))  
    }
      
    parsedExpressions ++ failures ++ invocationFailures
  }
  
  private def parseContext(context: Context)(implicit ctx: ParsingContext): ParseResult = 
  {
    val entries = context.getContextEntries.asScala
    val expressions = entries.map(_.getExpression)
    
    parseExpressions(expressions)
  }
  
  private def parseList(list: DmnList)(implicit ctx: ParsingContext): ParseResult = 
  {
    val expressions = list.getExpressions.asScala
    
    parseExpressions(expressions)
  }
  
  private def parseRelation(relation: Relation)(implicit ctx: ParsingContext): ParseResult = 
  {
    val columns = relation.getColumns.asScala  
    val rows = relation.getRows.asScala
    
    val failures = (List[Either[Failure, ParsedExpressionTuple]]() /: rows)( (failures, row) => 
      if (row.getExpressions.size != columns.size) {
        failures :+ Left(Failure(s"expected row with '${columns.size}' elements but found '${row.getExpressions.size}'"))
      } else {
        failures
      }
    )
    
    val expressions = rows.flatMap(_.getExpressions.asScala)
  
    parseExpressions(expressions) ++ failures
  }
  
  private def parseFunctionDefinition(functionDefinition: FunctionDefinition)(implicit ctx: ParsingContext): ParseResult = 
  {
    val expression = functionDefinition.getExpression
    
    expression match {
      case lt: LiteralExpression => parseLiteralExpression(lt)
      case other                 => List(Left(Failure(s"expected literal expression but found '$other'")))
    }
  }
  
  private def parseExpressions(expressions: Iterable[Expression])(implicit context: ParsingContext): ParseResult = 
  {
    (List[Either[Failure, ParsedExpressionTuple]]() /: expressions){ case (result, element) => {
      
      val parsedExpressions = result
        .filter(_.isRight)
        .map(_.right.get)
     
      val ctx = ParsingContext(
          context.model,
          context.parsedExpressions ++ getFeelExpressions(parsedExpressions), 
          context.parsedUnaryTest ++ getUnaryTests(parsedExpressions))
      
      val p = parseAnyExpression(element)
      
      result ++ p
    }}  
  }
  
  private def parseAnyExpression(expr: Expression)(implicit ctx: ParsingContext): ParseResult = 
  {
    expr match {
      case lt: LiteralExpression => parseLiteralExpression(lt)(ctx)
      case dt: DecisionTable     => parseDecisionTable(dt)(ctx)
      case inv: Invocation       => parseInvocation(inv)(ctx)
      case c: Context            => parseContext(c)(ctx)
      case l: DmnList            => parseList(l)(ctx)
      case rel: Relation         => parseRelation(rel)(ctx)
      case f: FunctionDefinition => parseFunctionDefinition(f)(ctx)
      case other                 => List( Left(Failure(s"unsupported expression found '$other'")) )
    }
  }
  
  private def parseExpression(expression: String): Either[Failure, (ParsedExpression, ExpressionType)] = {
    FeelParser.parseExpression(expression) match {
      case Success(exp, _) => Right(ParsedExpression(exp, expression), FeelExpression)
      case e: NoSuccess    => Left(Failure(s"Failed to parse FEEL expression '$expression':\n$e"))
    }
  }
  
  private def parseUnaryTests(expression: String): Either[Failure, (ParsedExpression, ExpressionType)] = {
    
    if (expression.isEmpty()) {
      Right(ParsedExpression(ConstBool(true), expression), UnaryTests)
    } else {
      FeelParser.parseUnaryTests(expression) match {
          case Success(exp, _) => Right(ParsedExpression(exp, expression), UnaryTests)
          case e: NoSuccess    => Left(Failure(s"Failed to parse FEEL unary-tests '$expression':\n$e"))
      }
    }
  }
}