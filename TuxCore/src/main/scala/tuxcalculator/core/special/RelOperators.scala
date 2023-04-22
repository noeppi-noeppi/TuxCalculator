package tuxcalculator.core.special

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.value.{MathBoolean, MathFalse, MathRealNumeric, MathValue, ValueHelper}

object RelOperators {
  
  object Eq extends CalculatorSpecial.SimpleFunction("eq", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      MathBoolean(ValueHelper.get(args(0)) == ValueHelper.get(args(1)))
    }
  }
  
  object Lt extends CalculatorSpecial.SimpleFunction("lt", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      (args(0), args(1)) match {
        case (MathRealNumeric(a), MathRealNumeric(b)) => MathBoolean(a < b)
        case _ => MathFalse
      }
    }
  }
}
