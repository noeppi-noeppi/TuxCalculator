package tuxcalculator.core.special

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.value._

object PolynomialOperators {
  
  object Pol extends CalculatorSpecial.SimpleFunction("pol", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      val list: Vector[MathValue] = ValueHelper.list(args(0))
      val coefficients = list.map(ValueHelper.complex).map(MathNumber.apply)
      MathPolynomial(coefficients)
    }
  }
  
  object Coeff extends CalculatorSpecial.SimpleFunction("coeff", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      args(0) match {
        case MathPolynomic(coefficients) => MathList(coefficients)
        case value => MathError("Not a polynomial: " + calc.format(value))
      }
    }
  }
}
