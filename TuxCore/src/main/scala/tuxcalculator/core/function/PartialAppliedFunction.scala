package tuxcalculator.core.function

import tuxcalculator.core.Calculator
import tuxcalculator.core.value.{MathFunction, MathValue}

class PartialAppliedFunction private(val value: MathValue, val partialArgs: Vector[MathValue]) extends MathFunction {
  override def string(calc: Calculator): String = value match {
    case _: LambdaFunction => "(" + calc.format(value) + ")_(" + partialArgs.map(calc.format).mkString(", ") + ")"
    case _ => calc.format(value) + "_(" + partialArgs.map(calc.format).mkString(", ") + ")"
  }
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = value.applyTo(calc, partialArgs ++ args)
}

object PartialAppliedFunction {
  def create(value: MathValue, args: Vector[MathValue]): PartialAppliedFunction = value match {
    case f: PartialAppliedFunction => new PartialAppliedFunction(f.value, f.partialArgs ++ args)
    case _ => new PartialAppliedFunction(value, args)
  }
}
