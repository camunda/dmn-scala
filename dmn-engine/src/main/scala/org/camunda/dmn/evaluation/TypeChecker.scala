package org.camunda.dmn.evaluation

import org.camunda.dmn.DmnEngine._
import org.camunda.feel._
import org.camunda.feel.interpreter._
  
object TypeChecker {
  
  val FEEL_PREFIX = "feel:"
  
  def isOfType(x: Val, t: String, context: EvalContext): Either[Failure, Val] = 
  {
    if (t == null || t.isEmpty) {
      Right(x)
    
    } else if (x == ValNull) {
      Right(x)
    
    } else {
      // accept type with and without prefix
      val `type` = t.replaceAll(FEEL_PREFIX, "")
      
      `type` match {
        case "string" => x match {
          case v: ValString => Right(v)
          case other        => Left(Failure(s"expected 'string' but found '$other'"))
        }
        case "number" => x match {
          case v: ValNumber => Right(v)
          case other        => Left(Failure(s"expected 'number' but found '$other'"))
        }
        case "boolean" => x match {
          case v: ValBoolean => Right(v)
          case other         => Left(Failure(s"expected 'boolean' but found '$other'"))
        }
        case "time" => x match {
          case v: ValTime      => Right(v)
          case v: ValLocalTime => Right(v)
          case other           => Left(Failure(s"expected 'time' but found '$other'"))
        }
        case "date" => x match {
          case v: ValDate     => Right(v)
          case other          => Left(Failure(s"expected 'date' but found '$other'"))
        }
        case "dateTime" => x match {
          case v: ValDateTime      => Right(v)
          case v: ValLocalDateTime => Right(v)
          case other               => Left(Failure(s"expected 'dateTime' but found '$other'"))
        }
        case "dayTimeDuration" => x match {
          case v: ValDayTimeDuration => Right(v)
          case other                 => Left(Failure(s"expected 'dayTimeDuration' but found '$other'"))
        }
        case "yearMonthDuration" => x match {
          case v: ValYearMonthDuration => Right(v)
          case other                   => Left(Failure(s"expected 'yearMonthDuration' but found '$other'"))
        }
        case other => Right(x) // ignore
      }
    }
  }
  
  def isOfType(x: Any, t: String, context: EvalContext): Either[Failure, Any] = 
  {
    if (t == null || t.isEmpty) {
      Right(x)
    
    } else if (x == null) {
      Right(x)
    
    } else {
      // accept type with and without prefix
      val `type` = t.replaceAll(FEEL_PREFIX, "")
      
      `type` match {
        case "string" => x match {
          case v: String => Right(v)
          case other        => Left(Failure(s"expected 'string' but found '$other'"))
        }
        case "number" => x match {
          case v: Number => Right(v)
          case other        => Left(Failure(s"expected 'number' but found '$other'"))
        }
        case "boolean" => x match {
          case v: Boolean => Right(v)
          case other         => Left(Failure(s"expected 'boolean' but found '$other'"))
        }
        case "time" => x match {
          case v: Time      => Right(v)
          case v: LocalTime => Right(v)
          case other           => Left(Failure(s"expected 'time' but found '$other'"))
        }
        case "date" => x match {
          case v: Date     => Right(v)
          case other          => Left(Failure(s"expected 'date' but found '$other'"))
        }
        case "dateTime" => x match {
          case v: DateTime      => Right(v)
          case v: LocalDateTime => Right(v)
          case other               => Left(Failure(s"expected 'dateTime' but found '$other'"))
        }
        case "dayTimeDuration" => x match {
          case v: DayTimeDuration => Right(v)
          case other                 => Left(Failure(s"expected 'dayTimeDuration' but found '$other'"))
        }
        case "yearMonthDuration" => x match {
          case v: YearMonthDuration    => Right(v)
          case other                   => Left(Failure(s"expected 'yearMonthDuration' but found '$other'"))
        }
        case other => Right(x) // ignore
      }
    }
  }
  
}