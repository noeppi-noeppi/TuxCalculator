package tuxcalculator.core.function

import tuxcalculator.core.Calculator
import tuxcalculator.core.lexer.FmtCode
import tuxcalculator.core.value.{MathError, MathFunction, MathValue}

import scala.collection.mutable.ListBuffer

class PartialAppliedFunction private(val value: MathValue, val partialArgs: Vector[Option[MathValue]]) extends MathFunction {
  override def string(calc: Calculator): String = {
    def argString(arg: Option[MathValue]): String = arg match {
      case Some(value) => calc.format(value)
      case None => "_"
    }
    value match {
      case _: LambdaFunction => calc.format(FmtCode.Open) + calc.format(value) + calc.format(FmtCode.Close) + calc.format(FmtCode.Partial) + calc.format(FmtCode.Open) + partialArgs.map(argString).mkString(", ") + calc.format(FmtCode.Close)
      case _ if partialArgs.contains(None) => calc.format(value) + calc.format(FmtCode.Open) + partialArgs.map(argString).mkString(calc.format(FmtCode.ElementSep)) + calc.format(FmtCode.Close)
      case _ => calc.format(value) + calc.format(FmtCode.Partial) + calc.format(FmtCode.Open) + partialArgs.map(argString).mkString(calc.format(FmtCode.ElementSep)) + calc.format(FmtCode.Close)
    }
  }
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = {
    val allArgs = ListBuffer[MathValue]()
    var argIdx = 0
    for (arg <- partialArgs) arg match {
      case Some(value) => allArgs.addOne(value)
      case None =>
        if (argIdx >= args.length) return MathError("Unfilled placeholder argument at position " + (allArgs.length + 1))
        allArgs.addOne(args(argIdx))
        argIdx += 1
    }
    allArgs.addAll(args.drop(argIdx))
    value.applyTo(calc, allArgs.toVector)
  }
}

object PartialAppliedFunction {
  def create(value: MathValue, args: Vector[Option[MathValue]]): PartialAppliedFunction = {
    def mergeArguments(first: List[Option[MathValue]], second: List[Option[MathValue]]): List[Option[MathValue]] = first match {
      case Some(arg) :: firstTail => Some(arg) :: mergeArguments(firstTail, second)
      case None :: firstTail => second match {
        case head :: secondTail => head :: mergeArguments(firstTail, secondTail)
        case Nil => None :: mergeArguments(firstTail, Nil)
      }
      case Nil => second
    }
    
    value match {
      case f: PartialAppliedFunction => new PartialAppliedFunction(f.value, mergeArguments(f.partialArgs.toList, args.toList).toVector.reverse.dropWhile(_.isEmpty).reverse)
      case _ => new PartialAppliedFunction(value, args.reverse.dropWhile(_.isEmpty).reverse)
    }
  }
}
