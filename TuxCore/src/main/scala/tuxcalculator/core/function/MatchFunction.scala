package tuxcalculator.core.function

import tuxcalculator.core.Calculator
import tuxcalculator.core.expression.Ast
import tuxcalculator.core.expression.Ast.Signature
import tuxcalculator.core.value._

import scala.annotation.tailrec

class MatchFunction(val entries: Vector[MatchFunctionEntry], val definitionCode: Ast.Expression) extends MathFunction {
  override def string(calc: Calculator): String = definitionCode.string(calc)
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = {
    @tailrec
    def applyToFirst(theEntries: List[MatchFunctionEntry]): MathValue = theEntries match {
      case head :: tail => head.tryApply(calc, args) match {
        case Some(result) => result
        case None => applyToFirst(tail)
      }
      case Nil => MathError("Function is not defined for arguments (" + args.map(arg => calc.format(arg)).mkString(", ") + ")")
    }
    applyToFirst(entries.toList)
  }
}

// mainGuard and code are lambda functions with the same signature.
// They can must called with unpacked varargs.
// element guards are 0 argument lambda functions that return the guard function
// this is to prevent them from being eagerly bound in non-eager mode.
class MatchFunctionEntry(val sig: Signature, val elementGuards: Vector[Option[MathFunction]], val mainGuard: Option[MathFunction], val code: MathFunction) {
  if (sig.names.length != elementGuards.length) throw new IllegalArgumentException("Guard count does not match signature. This is a bug.")
  
  def tryApply(calc: Calculator, args: Vector[MathValue]): Option[MathValue] = {
    LambdaFunction.makeArgValues(sig, args) match {
      case Some(argValues) =>
        @tailrec
        def checkElementGuards(theGuards: List[(MathValue, Option[MathFunction])]): Either[Boolean, MathValue] = theGuards match {
          case (value, guard) :: tail => checkGuard(calc, Vector(value), guard.map(_.applyTo(calc, Vector()))) match {
            case Left(true) => checkElementGuards(tail)
            case Left(false) => Left(false)
            case Right(err) => Right(err)
          }
          case Nil => Left(true)
        }

        checkElementGuards((argValues zip elementGuards).toList) match {
          case Left(true) => checkGuard(calc, args, mainGuard) match {
            case Left(true) => Some(code.applyTo(calc, args))
            case Left(false) => None
            case Right(err) => Some(err)
          }
          case Left(false) => None
          case Right(err) => Some(err)
        }
      case None => None
    }
  }
  
  private def checkGuard(calc: Calculator, values: Vector[MathValue], guard: Option[MathValue]): Either[Boolean, MathValue] = guard match {
    case Some(guardFunc) => guardFunc.applyTo(calc, values) match {
      case MathVoid => Right(MathVoid)
      case err: MathError => Right(err.trace("Used as guard with (" + values.map(calc.format).mkString(", ") + ")"))
      case resultValue => ValueHelper.make(calc) { ValueHelper.boolean(resultValue) }
    }
    case None => Left(true)
  }
}
