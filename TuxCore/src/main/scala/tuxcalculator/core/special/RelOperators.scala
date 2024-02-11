package tuxcalculator.core.special

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.{CalculatorSpecial, SpecialFunction}
import tuxcalculator.core.value.{MathBoolean, MathFalse, MathFunction, MathRealNumeric, MathValue, ValueHelper}

object RelOperators {
  
  object Eq extends CalculatorSpecial.SimpleFunction("eq", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      (ValueHelper.get(args(0)), ValueHelper.get(args(1))) match {
        case (f1: SpecialFunction, f2: SpecialFunction) => MathBoolean(f1 == f2) // Specials are unique
        // General functions can't be easily compared. For consistency they always yield false
        // Especially we don't compare global functions (references) because:
        //   1. We can't compare by name as the function might change after a `def' but the reference won't.
        //   2. We can't compare by value because then a reference loaded from a format file and a newly created one
        //      would not be equal.
        case (_: MathFunction, _: MathFunction) => MathFalse
        case (v1, v2) => MathBoolean(v1 == v2)
      }
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
