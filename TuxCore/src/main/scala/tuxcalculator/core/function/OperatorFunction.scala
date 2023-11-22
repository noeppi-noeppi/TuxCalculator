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

class MergedOperatorFunction(val name: String, val function1: MathFunction, val function2: MathFunction) extends MathFunction {
  override def string(calc: Calculator): String = name
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = args.length match {
    case 1 => function1.applyTo(calc, args)
    case 2 => function2.applyTo(calc, args)
    case _ => MathError("Operator '" + name + "' is not defined for " + args.length + " arguments")
  }
}
