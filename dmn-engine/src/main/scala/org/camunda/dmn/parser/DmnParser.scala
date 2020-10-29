package org.camunda.dmn.parser

import java.io.InputStream

import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.impl.DmnModelConstants
import org.camunda.bpm.model.dmn.instance.{
  BusinessKnowledgeModel,
  Context,
  Decision,
  DecisionTable,
  Expression,
  FunctionDefinition,
  Invocation,
  LiteralExpression,
  Relation,
  UnaryTests,
  Variable,
  List => DmnList
}
import org.camunda.dmn.DmnEngine.{Configuration, Failure}
import org.camunda.feel.impl.parser.FeelParser
import org.camunda.feel.syntaxtree._
import org.camunda.feel.impl.parser.FeelParser.{NoSuccess, Success}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Try

object DmnParser {

  val feelNameSpaces =
    List("feel", DmnModelConstants.FEEL_NS).map(_.toLowerCase())
}

class DmnParser(configuration: Configuration,
                parser: String => Either[String, ParsedExpression]) {

  import DmnParser._

  case class ParsingContext(model: DmnModelInstance) {

    val namesToEscape = getNamesToEscape(model)

    val parsedExpressions = mutable.Map[String, ParsedExpression]()
    val parsedUnaryTest = mutable.Map[String, ParsedExpression]()

    val decisions = mutable.Map[String, ParsedDecision]()
    val bkms = mutable.Map[String, ParsedBusinessKnowledgeModel]()

    val failures = mutable.ListBuffer[Failure]()
  }

  object ParsingFailure
      extends ParsedLiteralExpression(ParsedExpression(ConstNull, "<failure>"))

  object EmptyLogic
      extends ParsedLiteralExpression(ParsedExpression(ConstNull, "<empty>"))

  def parse(stream: InputStream): Either[Failure, ParsedDmn] = {

    Try(Dmn.readModelFromStream(stream)) match {
      case scala.util.Success(model) => {
        parseModel(model).left.map(failures =>
          Failure(failures.map(_.message).mkString("\n")))
      }
      case scala.util.Failure(e) => Left(Failure(s"Failed to parse DMN: $e"))
    }
  }

  private def parseModel(
      model: DmnModelInstance): Either[Iterable[Failure], ParsedDmn] = {

    val ctx = ParsingContext(model)

    val decisions = model.getDefinitions.getDrgElements.asScala
      .filter(_.isInstanceOf[Decision])

    decisions.map {
      case (d: Decision) =>
        ctx.decisions.getOrElseUpdate(d.getId, parseDecision(d)(ctx))
    }

    if (ctx.failures.isEmpty) {
      Right(ParsedDmn(model, ctx.decisions.values))
    } else {
      Left(ctx.failures)
    }
  }

  private def parseDecision(decision: Decision)(
      implicit
      ctx: ParsingContext): ParsedDecision = {

    // TODO be aware of loops
    val informationRequirements = decision.getInformationRequirements.asScala
    val requiredDecisions = informationRequirements
      .map(r => Option(r.getRequiredDecision))
      .flatten
      .map(d => ctx.decisions.getOrElseUpdate(d.getId, parseDecision(d)))

    val knowledgeRequirements = decision.getKnowledgeRequirements.asScala
    val requiredBkms = knowledgeRequirements
      .map(r => r.getRequiredKnowledge)
      .map(k =>
        ctx.bkms.getOrElseUpdate(k.getName, parseBusinessKnowledgeModel(k)))

    val logic: ParsedDecisionLogic = decision.getExpression match {
      case dt: DecisionTable     => parseDecisionTable(dt)
      case inv: Invocation       => parseInvocation(inv)
      case c: Context            => parseContext(c)
      case r: Relation           => parseRelation(r)
      case l: DmnList            => parseList(l)
      case lt: LiteralExpression => parseLiteralExpression(lt)
      case other => {
        ctx.failures += Failure(s"unsupported decision expression '$other'")
        ParsingFailure
      }
    }

    val variable = Option(decision.getVariable)
    val resultType = variable.flatMap(v => Option(v.getTypeRef))
    val resultName = variable
      .map(_.getName)
      .orElse(Option(decision.getId))
      .getOrElse(decision.getName)

    ParsedDecision(decision.getId,
                   decision.getName,
                   logic,
                   resultName,
                   resultType,
                   requiredDecisions,
                   requiredBkms)
  }

  private def parseBusinessKnowledgeModel(bkm: BusinessKnowledgeModel)(
      implicit
      ctx: ParsingContext): ParsedBusinessKnowledgeModel = {

    // TODO be aware of loops
    val knowledgeRequirements = bkm.getKnowledgeRequirement.asScala
    val requiredBkms = knowledgeRequirements
      .map(r => r.getRequiredKnowledge)
      .map(k =>
        ctx.bkms.getOrElseUpdate(k.getName, parseBusinessKnowledgeModel(k)))

    Option(bkm.getEncapsulatedLogic)
      .map { encapsulatedLogic =>
        val logic: ParsedDecisionLogic = encapsulatedLogic.getExpression match {
          case dt: DecisionTable     => parseDecisionTable(dt)
          case c: Context            => parseContext(c)
          case rel: Relation         => parseRelation(rel)
          case l: DmnList            => parseList(l)
          case lt: LiteralExpression => parseLiteralExpression(lt)
          case other => {
            ctx.failures += Failure(
              s"unsupported business knowledge model logic found '$other'")
            ParsingFailure
          }
        }

        val parameters = encapsulatedLogic.getFormalParameters.asScala
          .map(f => f.getName -> f.getTypeRef)

        ParsedBusinessKnowledgeModel(bkm.getId,
                                     bkm.getName,
                                     logic,
                                     parameters,
                                     requiredBkms)

      }
      .getOrElse {

        ParsedBusinessKnowledgeModel(bkm.getId,
                                     bkm.getName,
                                     EmptyLogic,
                                     Iterable.empty,
                                     requiredBkms)
      }
  }

  private def parseDecisionTable(decisionTable: DecisionTable)(
      implicit
      ctx: ParsingContext): ParsedDecisionTable = {

    if (decisionTable.getOutputs.size > 1 &&
        decisionTable.getHitPolicy.equals(HitPolicy.COLLECT) &&
        Option(decisionTable.getAggregation).isDefined) {
      ctx.failures += Failure(
        "hit policy 'COLLECT' with aggregator is not defined for compound output")
    }

    val inputExpressions = decisionTable.getInputs.asScala
      .map(
        i =>
          ParsedInput(i.getId,
                      i.getLabel,
                      parseFeelExpression(i.getInputExpression)))
    val rules = decisionTable.getRules.asScala
    val outputs = decisionTable.getOutputs.asScala

    val defaultOutputValues = outputs
      .map(
        o =>
          Option(o.getDefaultOutputEntry)
            .map(parseFeelExpression))

    val parsedOutputs = outputs.map(o => {
      val value = Option(o.getOutputValues).map(_.getText.getTextContent)
      val defaultValue =
        Option(o.getDefaultOutputEntry).map(parseFeelExpression)

      ParsedOutput(o.getId, o.getName, o.getLabel, value, defaultValue)
    })

    val parsedRules = rules.map(r => {
      val inputEntries = r.getInputEntries.asScala
        .map(parseUnaryTests)

      val outputNames = parsedOutputs.map(_.name)
      val outputEntries = r.getOutputEntries.asScala
        .map(parseFeelExpression)

      ParsedRule(r.getId, inputEntries, outputNames.zip(outputEntries))
    })

    ParsedDecisionTable(inputExpressions,
                        parsedOutputs,
                        parsedRules,
                        decisionTable.getHitPolicy,
                        decisionTable.getAggregation)
  }

  private def parseLiteralExpression(expression: LiteralExpression)(
      implicit
      ctx: ParsingContext): ParsedLiteralExpression = {
    val expr = parseFeelExpression(expression)

    ParsedLiteralExpression(expr)
  }

  private def parseContext(context: Context)(
      implicit
      ctx: ParsingContext): ParsedContext = {
    val entries = context.getContextEntries.asScala
    val lastEntry = entries.last

    // TODO verify that every entry has a variable name
    if (Option(lastEntry.getVariable).isDefined) {
      val contextEntries = entries.map(e =>
        e.getVariable.getName -> parseAnyExpression(e.getExpression))

      ParsedContext(contextEntries, None)
    } else {
      val contextEntries = entries
        .take(entries.size - 1)
        .map(e => e.getVariable.getName -> parseAnyExpression(e.getExpression))
      val aggregationEntry = parseAnyExpression(lastEntry.getExpression)

      ParsedContext(contextEntries, Some(aggregationEntry))
    }
  }

  private def parseList(list: DmnList)(implicit
                                       ctx: ParsingContext): ParsedList = {
    val entries = list.getExpressions.asScala
      .map(parseAnyExpression)

    ParsedList(entries)
  }

  private def parseRelation(relation: Relation)(
      implicit
      ctx: ParsingContext): ParsedRelation = {
    val rows = relation.getRows.asScala
    val columns = relation.getColumns.asScala
    val columNames = columns.map(_.getName)

    rows
      .filterNot(row => row.getExpressions.size == columns.size)
      .map(row => {
        ctx.failures += Failure(
          s"expected row with '${columns.size}' elements but found '${row.getExpressions.size}'")
      })

    val parsedRows = rows
      .map(_.getExpressions.asScala)
      .map(_.zip(columNames))
      .map(row =>
        ParsedRelationRow(row.map {
          case (expr, name) => name -> parseAnyExpression(expr)
        }))

    ParsedRelation(parsedRows)
  }

  private def parseFunctionDefinition(functionDefinition: FunctionDefinition)(
      implicit
      ctx: ParsingContext): ParsedDecisionLogic = {
    val expression = functionDefinition.getExpression
    val parameters = functionDefinition.getFormalParameters.asScala

    expression match {
      case lt: LiteralExpression => {
        val expr = parseFeelExpression(lt)
        val parametersWithTypes = parameters.map(p => p.getName -> p.getTypeRef)

        ParsedFunctionDefinition(expr, parametersWithTypes)
      }
      case other => {
        ctx.failures += Failure(
          s"expected literal expression but found '$other'")
        ParsingFailure
      }
    }
  }

  private def parseInvocation(invocation: Invocation)(
      implicit
      ctx: ParsingContext): ParsedDecisionLogic = {

    val bindings = invocation.getBindings.asScala
      .map(b =>
        b.getExpression match {
          case lt: LiteralExpression =>
            Some(b.getParameter.getName -> parseFeelExpression(lt))
          case other => {
            ctx.failures += Failure(
              s"expected binding with literal expression but found '$other'")

            None
          }
      })
      .flatten

    invocation.getExpression match {
      case lt: LiteralExpression => {
        val expression = lt.getText.getTextContent

        ctx.bkms
          .get(expression)
          .map(bkm => {
            ParsedInvocation(bindings, bkm)
          })
          .getOrElse {
            ctx.failures += Failure(s"no BKM found with name '$expression'")
            ParsingFailure
          }
      }
      case other => {
        ctx.failures += Failure(
          s"expected invocation with literal expression but found '$other'")
        ParsingFailure
      }
    }
  }

  private def parseAnyExpression(expr: Expression)(
      implicit
      ctx: ParsingContext): ParsedDecisionLogic = {
    expr match {
      case dt: DecisionTable     => parseDecisionTable(dt)(ctx)
      case inv: Invocation       => parseInvocation(inv)(ctx)
      case c: Context            => parseContext(c)(ctx)
      case rel: Relation         => parseRelation(rel)(ctx)
      case l: DmnList            => parseList(l)(ctx)
      case lt: LiteralExpression => parseLiteralExpression(lt)(ctx)
      case f: FunctionDefinition => parseFunctionDefinition(f)(ctx)
      case other => {
        ctx.failures += Failure(s"unsupported expression found '$other'")
        ParsingFailure
      }
    }
  }

  private def parseFeelExpression(lt: LiteralExpression)(
      implicit
      ctx: ParsingContext): ParsedExpression = {

    val expression = lt.getText.getTextContent

    val language =
      Option(lt.getExpressionLanguage).map(_.toLowerCase()).getOrElse("feel")
    if (!feelNameSpaces.contains(language)) {
      ctx.failures += Failure(
        s"Expression language '$language' is not supported")
      ParsedExpression(ConstNull, expression)

    } else {
      ctx.parsedExpressions.getOrElseUpdate(
        expression, {

          var escapedExpression =
            escapeNamesInExpression(expression, ctx.namesToEscape)

          parser(escapedExpression) match {
            case Right(parsedExpression) => parsedExpression
            case Left(failure) => {
              ctx.failures += Failure(
                s"Failed to parse FEEL expression '$expression': $failure")
              ParsedExpression(ConstNull, expression)
            }
          }
        }
      )
    }
  }

  private def parseUnaryTests(unaryTests: UnaryTests)(
      implicit
      ctx: ParsingContext): ParsedExpression = {

    val expression = unaryTests.getText.getTextContent

    val language = Option(unaryTests.getExpressionLanguage)
      .map(_.toLowerCase())
      .getOrElse("feel")
    if (!feelNameSpaces.contains(language)) {
      ctx.failures += Failure(
        s"Expression language '$language' is not supported")
      ParsedExpression(ConstNull, expression)

    } else {
      ctx.parsedUnaryTest.getOrElseUpdate(
        expression, {

          if (expression.isEmpty()) {
            ParsedExpression(ConstBool(true), expression)
          } else {

            var escapedExpression =
              escapeNamesInExpression(expression, ctx.namesToEscape)

            FeelParser.parseUnaryTests(escapedExpression) match {
              case Success(exp, _) => ParsedExpression(exp, expression)
              case e: NoSuccess => {
                ctx.failures += Failure(
                  s"Failed to parse FEEL unary-tests '$expression':\n$e")
                ParsedExpression(ConstNull, expression)
              }
            }
          }
        }
      )
    }
  }

  private def escapeNamesInExpression(
      expression: String,
      namesWithSpaces: Iterable[String]): String = {

    (expression /: namesWithSpaces)(
      (e, name) =>
        e.replaceAll("""([(,.]|\s|^)(""" + name + """)([(),.]|\s|$)""",
                     "$1'$2'$3"))
  }

  private def getNamesToEscape(model: DmnModelInstance): Iterable[String] = {

    val variables = model.getModelElementsByType(classOf[Variable])
    val variableNames = variables.asScala.map(_.getName)

    val nameFilter: (String => Boolean) = {
      if (configuration.escapeNamesWithSpaces && configuration.escapeNamesWithDashes) {
        name =>
          name.contains(" ") || name.contains("-")
      } else if (configuration.escapeNamesWithSpaces) { name =>
        name.contains(" ")
      } else if (configuration.escapeNamesWithDashes) { name =>
        name.contains("-")
      } else { name =>
        false
      }
    }

    val namesToEscape = variableNames.filter(nameFilter)
    namesToEscape.toList.distinct
      .sortBy(_.length)
      .reverse
  }

}
