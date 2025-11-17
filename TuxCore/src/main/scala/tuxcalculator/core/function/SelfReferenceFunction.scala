package tuxcalculator.core.function

import tuxcalculator.core.Calculator
import tuxcalculator.core.lexer.FmtCode
import tuxcalculator.core.value.{MathFunction, MathValue}

class SelfReferenceFunction(val value: MathValue) extends MathFunction {

  override def string(calc: Calculator): String = calc.format(FmtCode.SelfReference) + calc.format(value)
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = value.applyTo(calc, Vector(this) ++ args)
}
