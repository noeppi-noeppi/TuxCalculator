package tuxcalculator.core.function

import tuxcalculator.core.Calculator
import tuxcalculator.core.value.{MathError, MathFunction, MathValue}

class OperatorFunction(val name: String, val function: MathFunction) extends MathFunction {
  override def string(calc: Calculator): String = name
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = function.applyTo(calc, args)
}

class BracketFunction(val name: String, val close: String, val function: MathFunction) extends MathFunction {
  override def string(calc: Calculator): String = name + " ... " + close
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = function.applyTo(calc, args)
}

class ChainedOperatorFunction(val name: String, val priority: Int, val function1: Option[MathFunction], val functionN: Option[MathFunction]) extends MathFunction {
  override def string(calc: Calculator): String = name
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = args.length match {
    case 1 if function1.isDefined => function1.get.applyTo(calc, args)
    case n if n >= 2 && priority % 2 == 0 && functionN.isDefined => args.reduceLeft((v1, v2) => functionN.get.applyTo(calc, Vector(v1, v2)))
    case n if n >= 2 && priority % 2 != 0 && functionN.isDefined => args.reduceRight((v1, v2) => functionN.get.applyTo(calc, Vector(v1, v2)))
    case _ => MathError("Operator '" + name + "' is not defined for " + args.length + " arguments")
  }
}
