/*
 * Copyright © 2022 Camunda Services GmbH (info@camunda.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.camunda.dmn.parser

import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.impl.DmnModelConstants
import org.camunda.bpm.model.dmn.instance.BusinessKnowledgeModel
import org.camunda.bpm.model.dmn.instance.Column
import org.camunda.bpm.model.dmn.instance.Context
import org.camunda.bpm.model.dmn.instance.Decision
import org.camunda.bpm.model.dmn.instance.DecisionTable
import org.camunda.bpm.model.dmn.instance.Definitions
import org.camunda.bpm.model.dmn.instance.DrgElement
import org.camunda.bpm.model.dmn.instance.Expression
import org.camunda.bpm.model.dmn.instance.FunctionDefinition
import org.camunda.bpm.model.dmn.instance.InformationItem
import org.camunda.bpm.model.dmn.instance.InformationRequirement
import org.camunda.bpm.model.dmn.instance.Invocation
import org.camunda.bpm.model.dmn.instance.ItemDefinition
import org.camunda.bpm.model.dmn.instance.KnowledgeRequirement
import org.camunda.bpm.model.dmn.instance.LiteralExpression
import org.camunda.bpm.model.dmn.instance.Relation
import org.camunda.bpm.model.dmn.instance.RequiredDecisionReference
import org.camunda.bpm.model.dmn.instance.RequiredKnowledgeReference
import org.camunda.bpm.model.dmn.instance.UnaryTests
import org.camunda.bpm.model.dmn.instance.{List => DmnList}
import org.camunda.bpm.model.xml.instance.ModelElementInstance
import org.camunda.dmn.DmnEngine.Configuration
import org.camunda.dmn.DmnEngine.Failure
import org.camunda.dmn.logger
import org.camunda.feel

import java.io.InputStream
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

object DmnParser {

  val feelNameSpaces: List[String] = List(
    "feel",
    DmnModelConstants.FEEL_NS,
    DmnModelConstants.FEEL12_NS,
    DmnModelConstants.FEEL13_NS,
    DmnModelConstants.FEEL14_NS,
    DmnModelConstants.DMN15_NS
  ).map(_.toLowerCase())

  val dmnNamespaces: List[String] = List(
    DmnModelConstants.DMN11_NS,
    DmnModelConstants.DMN11_ALTERNATIVE_NS,
    DmnModelConstants.DMN12_NS,
    DmnModelConstants.DMN13_NS,
    DmnModelConstants.DMN13_ALTERNATIVE_NS
  ).map(_.toLowerCase())
}

class DmnParser(
  configuration: Configuration,
  feelParser: String => Either[String, feel.syntaxtree.ParsedExpression],
  feelUnaryTestsParser: String => Either[String,
    feel.syntaxtree.ParsedExpression]) {

  import DmnParser._

  case class ImportedModel(namespace: String, name: String)

  case class ModelReference(namespace: String, id: String) {
    def isEmbedded: Boolean = namespace.isEmpty

    def isImported: Boolean = !isEmbedded
  }

  case class ParsingContext(model: DmnModelInstance) {

    val namesToEscape = getNamesToEscape(model)

    val parsedFeelExpressions = mutable.Map[String, ParsedExpression]()
    val parsedFeelUnaryTest = mutable.Map[String, ParsedExpression]()

    val decisions = mutable.Map[String, ParsedDecisionReference]()
    val bkms = mutable.Map[String, ParsedBusinessKnowledgeModelReference]()

    val importedModels = mutable.ListBuffer[ImportedModel]()

    val failures = mutable.ListBuffer[Failure]()
  }

  object ParsingFailure
    extends ParsedLiteralExpression(ExpressionFailure("<failure>"))

  object EmptyLogic
    extends ParsedLiteralExpression(
      FeelExpression(
        feel.syntaxtree.ParsedExpression(expression =
          feel.syntaxtree.ConstNull,
          text = "<empty>")
      ))

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

    val definitions = model.getDefinitions

    val importedModels = getImportedModels(definitions)
    ctx.importedModels.addAll(importedModels)

    val drgElements = definitions.getDrgElements.asScala

    checkForCyclicDependencies(drgElements) match {
      case Left(failure) => Left(List(failure))
      case _ =>

        // parse decisions and BKMs
        drgElements.map {
          case decision: Decision => ctx.decisions.getOrElseUpdate(decision.getId, parseDecision(decision)(ctx))
          case bkm: BusinessKnowledgeModel => ctx.bkms.getOrElseUpdate(bkm.getName, parseBusinessKnowledgeModel(bkm)(ctx))
          case _ => // ignore
        }

        val parsedDmn = ParsedDmn(
          model = model,
          decisions = ctx.decisions.values.collect{ case decision: ParsedDecision => decision },
          bkms = ctx.bkms.values.collect { case bkm: ParsedBusinessKnowledgeModel => bkm },
          namespace = definitions.getNamespace)

        if (ctx.failures.isEmpty) {
          Right(parsedDmn)

        } else if (configuration.lazyEvaluation) {
          logger.warn("Parsing the DMN reported the following failures:\n{}",
            ctx.failures.map(_.message).mkString("\n"))
          Right(parsedDmn)
        } else {
          Left(ctx.failures)
        }
    }
  }

  private def getImportedModels(definitions: Definitions): Iterable[ImportedModel] = {
    definitions.getImports.asScala
      .filter { anImport =>
        val importType = anImport.getImportType.toLowerCase
        dmnNamespaces.contains(importType)
      }
      .map { anImport =>
        ImportedModel(
          namespace = anImport.getNamespace,
          name = anImport.getAttributeValue("name")
        )
      }
  }

  private def checkForCyclicDependencies(drgElements: Iterable[DrgElement]): Either[Failure, Unit] = {
    val decisions = drgElements.collect { case d: Decision => d }
    val bkms = drgElements.collect { case b: BusinessKnowledgeModel => b }

    if (hasCyclicDependenciesInDecisions(decisions)) {
      Left(Failure(
        "Invalid DMN model: Cyclic dependencies between decisions detected."))

    } else if (hasCyclicDependenciesInBkms(bkms)) {
      Left(Failure(
        "Invalid DMN model: Cyclic dependencies between BKMs detected."))

    } else {
      // no cyclic dependencies found
      Right()
    }
  }

  private def hasCyclicDependenciesInDecisions(decisions: Iterable[Decision]): Boolean = {
    val dependencies = decisions.map { decision =>
      val requiredDecisions = decision.getInformationRequirements.asScala
        .filter(requirement => getDecisionReference(requirement).exists(_.isEmbedded))
        .flatMap(requirement => Option(requirement.getRequiredDecision).map(_.getId))

      decision.getId -> requiredDecisions
    }.toMap

    decisions.exists(decision =>
      hasDependencyCycle(
        visit = decision.getId,
        visited = Set.empty,
        dependencies = dependencies
      ))
  }

  private def hasCyclicDependenciesInBkms(bkms: Iterable[BusinessKnowledgeModel]): Boolean = {
    val dependencies = bkms.map { bkm =>
      val requiredBkms = bkm.getKnowledgeRequirement.asScala
        .filter(requirement => getBkmReference(requirement).exists(_.isEmbedded))
        .flatMap(requirement => Option(requirement.getRequiredKnowledge).map(_.getId))

      bkm.getId -> requiredBkms
    }.toMap

    bkms.exists(bkm =>
      hasDependencyCycle(
        visit = bkm.getId,
        visited = Set.empty,
        dependencies = dependencies
      ))
  }

  private def hasDependencyCycle(visit: String,
                                 visited: Set[String],
                                 dependencies: Map[String, Iterable[String]]): Boolean = {
    if (visited.contains(visit)) {
      true
    } else {
      dependencies.getOrElse(visit, Nil).exists(dependency =>
        hasDependencyCycle(
          visit = dependency,
          visited = visited + visit,
          dependencies = dependencies
        )
      )
    }
  }

  private def parseDecision(decision: Decision)(
    implicit
    ctx: ParsingContext): ParsedDecisionReference = {

    val requiredDecisions = decision.getInformationRequirements.asScala
      .flatMap(requirement =>
        getDecisionReference(requirement).map(reference => (requirement, reference))
      )
      .map { case (requirement, reference) => parseRequiredDecision(requirement, reference) }

    val requiredBkms = decision.getKnowledgeRequirements.asScala
      .flatMap(requirement =>
        getBkmReference(requirement).map(reference => (requirement, reference))
      )
      .map { case (requirement, reference) => parseRequiredBkm(requirement, reference) }

    val logic: ParsedDecisionLogic = decision.getExpression match {
      case dt: DecisionTable => parseDecisionTable(dt)
      case inv: Invocation => parseInvocation(inv)
      case c: Context => parseContext(c)
      case r: Relation => parseRelation(r)
      case l: DmnList => parseList(l)
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

    EmbeddedDecision(
      id = decision.getId,
      name = decision.getName,
      logic = logic,
      resultName = resultName,
      resultType = resultType,
      requiredDecisions = requiredDecisions,
      requiredBkms = requiredBkms
    )
  }

  private def getDecisionReference(informationRequirement: InformationRequirement): Option[ModelReference] = {
    Option(informationRequirement.getUniqueChildElementByType(classOf[RequiredDecisionReference]))
      .map(createModelReference)
  }

  private def createModelReference(elementReference: ModelElementInstance): ModelReference = {
    val href = elementReference.getAttributeValue("href")
    val index = Math.max(href.indexOf("#"), 0)

    ModelReference(
      namespace = href.substring(0, index),
      id = href.substring(index + 1)
    )
  }

  private def parseRequiredDecision(informationRequirement: InformationRequirement, reference: ModelReference)(implicit ctx: ParsingContext): ParsedDecisionReference = {
    if (reference.isEmbedded) {
      val requiredDecision = informationRequirement.getRequiredDecision
      ctx.decisions.getOrElseUpdate(requiredDecision.getId, parseDecision(decision = requiredDecision))
    } else {
      ctx.importedModels
        .find(importedModel => reference.namespace == importedModel.namespace)
        .map(importedModel => ImportedDecision(reference.namespace, reference.id, importedModel.name))
        .getOrElse {
          val failure = Failure(s"No import found for namespace '${reference.namespace}'.")
          ctx.failures += failure
          ParsedDecisionFailure(reference.id, reference.namespace, failure.message)
        }
    }
  }

  private def getBkmReference(knowledgeRequirement: KnowledgeRequirement): Option[ModelReference] = {
    Option(knowledgeRequirement.getUniqueChildElementByType(classOf[RequiredKnowledgeReference]))
      .map(createModelReference)
  }

  private def parseRequiredBkm(knowledgeRequirement: KnowledgeRequirement, reference: ModelReference)(implicit ctx: ParsingContext): ParsedBusinessKnowledgeModelReference = {
    if (reference.isEmbedded) {
      val requiredKnowledge = knowledgeRequirement.getRequiredKnowledge
      ctx.bkms.getOrElseUpdate(requiredKnowledge.getName, parseBusinessKnowledgeModel(requiredKnowledge))
    } else {
      ctx.importedModels
        .find(importedModel => reference.namespace == importedModel.namespace)
        .map(importedModel => ImportedBusinessKnowledgeModel(reference.namespace, reference.id, importedModel.name))
        .getOrElse {
          val failure = Failure(s"No import found for namespace '${reference.namespace}'.")
          ctx.failures += failure
          ParsedBusinessKnowledgeModelFailure(reference.id, reference.namespace, failure.message)
        }
    }
  }

  private def parseBusinessKnowledgeModel(bkm: BusinessKnowledgeModel)(
    implicit
    ctx: ParsingContext): ParsedBusinessKnowledgeModelReference = {

    // TODO be aware of loops
    val knowledgeRequirements = bkm.getKnowledgeRequirement.asScala
    val requiredBkms = knowledgeRequirements
      .map(r => r.getRequiredKnowledge)
      .map(k =>
        ctx.bkms.getOrElseUpdate(k.getName, parseBusinessKnowledgeModel(k)))

    Option(bkm.getEncapsulatedLogic)
      .map { encapsulatedLogic =>
        val logic: ParsedDecisionLogic = encapsulatedLogic.getExpression match {
          case dt: DecisionTable => parseDecisionTable(dt)
          case c: Context => parseContext(c)
          case rel: Relation => parseRelation(rel)
          case l: DmnList => parseList(l)
          case lt: LiteralExpression => parseLiteralExpression(lt)
          case other => {
            ctx.failures += Failure(
              s"unsupported business knowledge model logic found '$other'")
            ParsingFailure
          }
        }

        val parameters = encapsulatedLogic.getFormalParameters.asScala
          .map(f => f.getName -> f.getTypeRef)

        EmbeddedBusinessKnowledgeModel(bkm.getId,
          bkm.getName,
          logic,
          parameters,
          requiredBkms)

      }
      .getOrElse {

        EmbeddedBusinessKnowledgeModel(bkm.getId,
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

    if (decisionTable.getOutputs.size > 1) {
      decisionTable.getOutputs
        .stream()
        .filter(output => output.getName == null)
        .forEach(output =>
          ctx.failures += Failure(
            s"no output name defined for `${output.getLabel}`"))
    }

    val inputExpressions = decisionTable.getInputs.asScala
      .map(
        i =>
          ParsedInput(i.getId,
            i.getLabel,
            parseFeelExpression(i.getInputExpression)))
    val rules = decisionTable.getRules.asScala
    val outputs = decisionTable.getOutputs.asScala

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
      .flatMap(b =>
        b.getExpression match {
          case lt: LiteralExpression =>
            Some(b.getParameter.getName -> parseFeelExpression(lt))
          case other => {
            ctx.failures += Failure(
              s"expected binding with literal expression but found '$other'")
            None
          }
        })

    invocation.getExpression match {
      case lt: LiteralExpression => {
        val expression = lt.getText.getTextContent

        ctx.bkms
          .get(expression)
          .map(bkmRef => {
            ParsedInvocation(bindings, bkmRef)
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
      case dt: DecisionTable => parseDecisionTable(dt)(ctx)
      case inv: Invocation => parseInvocation(inv)(ctx)
      case c: Context => parseContext(c)(ctx)
      case rel: Relation => parseRelation(rel)(ctx)
      case l: DmnList => parseList(l)(ctx)
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

    val result = for {
      expression <- validateNotEmpty(lt)
      _ <- validateExpressionLanguage(lt)
    } yield parseFeelExpression(expression)

    result match {
      case Right(expression) => expression
      case Left(failure) =>
        ctx.failures += failure
        ExpressionFailure(failure = failure.message)
    }
  }

  private def validateNotEmpty(lt: LiteralExpression): Either[Failure, String] =
    Option(lt.getText)
      .map(_.getTextContent)
      .toRight(Failure(s"The expression '${lt.getId}' must not be empty."))

  private def validateExpressionLanguage(lt: LiteralExpression): Either[Failure, Unit] = {
    val language =
      Option(lt.getExpressionLanguage).map(_.toLowerCase).getOrElse("feel")
    if (feelNameSpaces.contains(language)) {
      Right(())
    } else {
      Left(Failure(s"Expression language '$language' is not supported"))
    }
  }

  private def parseFeelExpression(expression: String)(
    implicit ctx: ParsingContext): ParsedExpression = {
    ctx.parsedFeelExpressions.getOrElseUpdate(
      expression, {
        val escapedExpression =
          escapeNamesInExpression(expression, ctx.namesToEscape)

        feelParser(escapedExpression) match {
          case Right(parsedExpression) =>
            FeelExpression(expression = parsedExpression)
          case Left(failure) =>
            ctx.failures += Failure(s"FEEL expression: $failure")
            ExpressionFailure(failure = s"FEEL expression: $failure")
        }
      }
    )
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
      ExpressionFailure(
        failure = s"Expression language '$language' is not supported")

    } else {
      ctx.parsedFeelUnaryTest.getOrElseUpdate(
        expression, {

          if (expression.isEmpty) {
            EmptyExpression
          } else {

            val escapedExpression =
              escapeNamesInExpression(expression, ctx.namesToEscape)

            feelUnaryTestsParser(escapedExpression) match {
              case Right(parsedExpression) =>
                FeelExpression(expression = parsedExpression)
              case Left(failure) => {
                ctx.failures += Failure(s"FEEL unary-tests: $failure")
                ExpressionFailure(failure = s"FEEL unary-tests: $failure")
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
          "$1`$2`$3"))
  }

  private def getNamesToEscape(model: DmnModelInstance): Iterable[String] = {

    val names = Seq(classOf[InformationItem], classOf[ItemDefinition]).flatMap(elementType =>
      model.getModelElementsByType(elementType).asScala)
      .filterNot(classOf[Column].isInstance(_))
      .map(_.getName)

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

    val namesToEscape = names.filter(nameFilter)
    namesToEscape.toList.distinct
      .sortBy(_.length)
      .reverse
  }

}
