package org.camunda.dmn.evaluation


import org.camunda.dmn.DmnEngine._
import org.camunda.feel._
import org.camunda.feel.interpreter._
  
object TypeChecker {
  
  def isOfType(x: Val, t: String, context: EvalContext): Either[Failure, Val] = 
  {
    t match {
      case "feel:string" => x match {
        case v: ValString => Right(v)
        case other        => Left(Failure(s"expected 'feel:string' but found '$other'"))
      }
      case "feel:number" => x match {
        case v: ValNumber => Right(v)
        case other        => Left(Failure(s"expected 'feel:number' but found '$other'"))
      }
      case "feel:boolean" => x match {
        case v: ValBoolean => Right(v)
        case other         => Left(Failure(s"expected 'feel:boolean' but found '$other'"))
      }
      case "feel:time" => x match {
        case v: ValTime      => Right(v)
        case v: ValLocalTime => Right(v)
        case other           => Left(Failure(s"expected 'feel:time' but found '$other'"))
      }
      case "feel:date" => x match {
        case v: ValDate     => Right(v)
        case other          => Left(Failure(s"expected 'feel:date' but found '$other'"))
      }
      case "feel:dateTime" => x match {
        case v: ValDateTime      => Right(v)
        case v: ValLocalDateTime => Right(v)
        case other               => Left(Failure(s"expected 'feel:dateTime' but found '$other'"))
      }
      case "feel:dayTimeDuration" => x match {
        case v: ValDayTimeDuration => Right(v)
        case other                 => Left(Failure(s"expected 'feel:dayTimeDuration' but found '$other'"))
      }
      case "feel:yearMonthDuration" => x match {
        case v: ValYearMonthDuration => Right(v)
        case other                   => Left(Failure(s"expected 'feel:yearMonthDuration' but found '$other'"))
      }
      case other => Right(x) // ignore
    }
  }
  
  def isOfType(x: Any, t: String, context: EvalContext): Either[Failure, Any] = 
  {
    t match {
      case "feel:string" => x match {
        case v: String => Right(v)
        case other        => Left(Failure(s"expected 'feel:string' but found '$other'"))
      }
      case "feel:number" => x match {
        case v: Number => Right(v)
        case other        => Left(Failure(s"expected 'feel:number' but found '$other'"))
      }
      case "feel:boolean" => x match {
        case v: Boolean => Right(v)
        case other         => Left(Failure(s"expected 'feel:boolean' but found '$other'"))
      }
      case "feel:time" => x match {
        case v: Time      => Right(v)
        case v: LocalTime => Right(v)
        case other           => Left(Failure(s"expected 'feel:time' but found '$other'"))
      }
      case "feel:date" => x match {
        case v: Date     => Right(v)
        case other          => Left(Failure(s"expected 'feel:date' but found '$other'"))
      }
      case "feel:dateTime" => x match {
        case v: DateTime      => Right(v)
        case v: LocalDateTime => Right(v)
        case other               => Left(Failure(s"expected 'feel:dateTime' but found '$other'"))
      }
      case "feel:dayTimeDuration" => x match {
        case v: DayTimeDuration => Right(v)
        case other                 => Left(Failure(s"expected 'feel:dayTimeDuration' but found '$other'"))
      }
      case "feel:yearMonthDuration" => x match {
        case v: YearMonthDuration    => Right(v)
        case other                   => Left(Failure(s"expected 'feel:yearMonthDuration' but found '$other'"))
      }
      case other => Right(x) // ignore
    }
  }
  
}