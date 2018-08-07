package org.camunda.dmn.evaluation

import scala.collection.JavaConverters._

import org.camunda.dmn.DmnEngine._
import org.camunda.dmn.FunctionalHelper._
import org.camunda.feel._
import org.camunda.feel.interpreter.{
  RootContext,
  Val,
  ValNull,
  ValContext,
  DefaultContext,
  ValList,
  ValNumber,
  ValBoolean,
  ValString
}
import org.camunda.bpm.model.dmn._
import org.camunda.bpm.model.dmn.instance.{
  Decision,
  DecisionTable,
  InputEntry,
  OutputEntry,
  Output,
  Rule,
  Input,
  LiteralExpression,
  UnaryTests
}
import org.camunda.dmn.parser.{ParsedDecisionTable, ParsedRule, ParsedOutput}

class DecisionTableEvaluator(
    eval: (ParsedExpression, EvalContext) => Either[Failure, Val]) {

  def eval(decisionTable: ParsedDecisionTable,
           context: EvalContext): Either[Failure, Val] = {
    implicit val ctx = context

    evalInputExpressions(decisionTable.inputs).right.flatMap { inputValues =>
      checkRules(decisionTable.rules, inputValues).right
        .map(_.flatten)
        .flatMap { matchedRules =>
          matchedRules match {
            case Nil => applyDefaultOutputEntries(decisionTable.outputs)
            case _ => {

              val rules =
                if (decisionTable.hitPolicy == HitPolicy.FIRST)
                  List(matchedRules.head)
                else matchedRules

              evalOutputValues(rules).right.flatMap(
                values =>
                  applyHitPolicy(decisionTable.hitPolicy,
                                 decisionTable.aggregation,
                                 decisionTable.outputs,
                                 values))
            }
          }
        }
    }
  }

  private def evalInputExpressions(inputs: Iterable[ParsedExpression])(
      implicit
      context: EvalContext): Either[Failure, List[Any]] = {
    mapEither(inputs, (input: ParsedExpression) => eval(input, context))
  }

  private def checkRules(rules: Iterable[ParsedRule], inputValues: List[Any])(
      implicit
      context: EvalContext): Either[Failure, List[Option[ParsedRule]]] = {
    mapEither(rules, (rule: ParsedRule) => {

      evalInputEntries(rule.inputEntries.zip(inputValues)).right
        .map(isMet => if (isMet) Some(rule) else None)
    })
  }

  private def evalInputEntries(inputEntries: Iterable[(ParsedExpression, Any)])(
      implicit
      context: EvalContext): Either[Failure, Boolean] = {

    inputEntries match {
      case Nil => Right(true)
      case (entry, value) :: is => {

        evalInputEntry(entry, value).right
          .flatMap { result =>
            result match {
              case ValBoolean(false) => Right(false)
              case ValBoolean(true)  => evalInputEntries(is)
              case other =>
                Left(Failure(
                  s"input entry must return true or false, but found '$other'"))
            }
          }
      }
    }
  }

  private def evalInputEntry(entry: ParsedExpression, inputValue: Any)(
      implicit
      context: EvalContext): Either[Failure, Val] = {

    val variablesWithInput = context.variables + (RootContext.defaultInputVariable -> inputValue)

    eval(entry, context.copy(variables = variablesWithInput))
  }

  private def applyDefaultOutputEntries(outputs: Iterable[ParsedOutput])(
      implicit
      context: EvalContext): Either[Failure, Val] = {

    evalDefaultOutputEntries(outputs).right
      .map(_.flatten.toMap)
      .map { outputValues =>
        if (outputValues.isEmpty) {
          ValNull
        } else if (outputValues.size == 1) {
          outputValues.values.head
        } else {
          ValContext(DefaultContext(outputValues))
        }
      }
  }

  private def evalDefaultOutputEntries(outputs: Iterable[ParsedOutput])(
      implicit
      context: EvalContext): Either[Failure, List[Option[(String, Val)]]] = {
    mapEither(outputs, (output: ParsedOutput) => {

      output.defaultValue
        .map(expr =>
          eval(expr, context).right
            .map(r => Some(output.name -> r)))
        .getOrElse(Right(None))
    })
  }

  private def evalOutputValues(rules: Iterable[ParsedRule])(
      implicit
      context: EvalContext): Either[Failure, List[Map[String, Val]]] = {

    mapEither(
      rules,
      (rule: ParsedRule) => {

        mapEither[(String, ParsedExpression), (String, Val)](
          rule.outputEntries, {
            case (name, expr) =>
              eval(expr, context).right
                .map(name -> _)
          }).right.map(_.toMap)
      }
    )
  }

  private def applyHitPolicy(
      hitPolicy: HitPolicy,
      aggregator: BuiltinAggregator,
      outputs: Iterable[ParsedOutput],
      outputValues: List[Map[String, Val]]): Either[Failure, Val] = {

    Option(hitPolicy).getOrElse(HitPolicy.UNIQUE) match {

      case HitPolicy.FIRST => Right(singleOutputValue(outputValues))

      case HitPolicy.UNIQUE => {

        if (outputValues.isEmpty || outputValues.size == 1) {
          Right(singleOutputValue(outputValues))
        } else {
          Left(Failure(
            s"multiple values aren't allowed for UNIQUE hit policy. found: '$outputValues'"))
        }
      }

      case HitPolicy.ANY => {

        val disinctValues = outputValues.distinct

        if (disinctValues.isEmpty || disinctValues.size == 1) {
          Right(singleOutputValue(outputValues))
        } else {
          Left(Failure(
            s"different values aren't allowed for ANY hit policy. found: '$disinctValues'"))
        }
      }

      case HitPolicy.PRIORITY =>
        Right(singleOutputValue(sortByPriority(outputValues, outputs)))

      case HitPolicy.OUTPUT_ORDER =>
        Right(multipleOutputValues(sortByPriority(outputValues, outputs)))

      case HitPolicy.RULE_ORDER => Right(multipleOutputValues(outputValues))

      case HitPolicy.COLLECT =>
        aggregator match {
          case BuiltinAggregator.MIN =>
            singleNumberValues(outputValues).right.map(r => ValNumber(r.min))
          case BuiltinAggregator.MAX =>
            singleNumberValues(outputValues).right.map(r => ValNumber(r.max))
          case BuiltinAggregator.SUM =>
            singleNumberValues(outputValues).right.map(r => ValNumber(r.sum))
          case BuiltinAggregator.COUNT => Right(ValNumber(outputValues.size))
          case _                       => Right(multipleOutputValues(outputValues))
        }
    }
  }

  private def singleOutputValue(values: List[Map[String, Val]]): Val = {
    values.headOption
      .map(v =>
        if (v.size == 1) v.values.head else ValContext(DefaultContext(v)))
      .getOrElse(ValNull)
  }

  private def multipleOutputValues(values: List[Map[String, Val]]): Val =
    values match {
      case Nil                           => ValNull
      case list if (list.head.size == 1) => ValList(list.map(_.values.head))
      case list                          => ValList(list.map(m => ValContext(DefaultContext(m))))
    }

  private def sortByPriority(
      outputValues: List[Map[String, Val]],
      outputs: Iterable[ParsedOutput]): List[Map[String, Val]] = {

    val priorities: Iterable[(String, Map[String, Int])] = outputs.map {
      output =>
        val values = output.value
          .map(_.split(",").map(_.trim.replaceAll("\"|'", ""))) // remove string quotes and whitespaces
          .map(_.toList)
          .getOrElse(List())

        output.name -> values.zipWithIndex.toMap
    }

    outputValues.sortBy { values =>
      val valuePriorities = priorities.map {
        case (output, priority) =>
          values(output) match {
            case ValString(value) =>
              priority.get(value).map(_.toString).getOrElse("_")
            case ValNumber(value) =>
              priority.get(value.toString).map(_.toString).getOrElse("_")
            case _ => "_" // priority doesn't work for other types
          }
      }

      valuePriorities.reduce(_ + _)
    }
  }

  private def singleNumberValues(
      values: List[Map[String, Val]]): Either[Failure, List[Number]] =
    values match {
      case Nil => Right(Nil)
      case list if (list.head.size == 1) =>
        mapEither(list.map(_.values.head), numberValue)
      case list =>
        Left(Failure(s"multiple values aren't allowed. found: $list"))
    }

  private def numberValue(value: Val): Either[Failure, Number] = value match {
    case n: ValNumber => Right(n.value)
    case o            => Left(Failure(s"expected number but found '$o'"))
  }

}
