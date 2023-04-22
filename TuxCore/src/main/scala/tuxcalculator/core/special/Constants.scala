package tuxcalculator.core.special

import ch.obermuhlner.math.big.{BigComplex, BigDecimalMath}
import tuxcalculator.core.data.{CalculatorSpecial, PropertyAccess}
import tuxcalculator.core.value.{MathFalse, MathNumber, MathTrue, MathValue, MathVoid}

object Constants {

  object True extends CalculatorSpecial.Constant("t") {
    override def value: MathValue = MathTrue
  }
  
  object False extends CalculatorSpecial.Constant("f") {
    override def value: MathValue = MathFalse
  }
  
  object Void extends CalculatorSpecial.Constant("v") {
    override def value: MathValue = MathVoid
  }
  
  object Pi extends CalculatorSpecial.Dynamic("pi") {
    override def value(calc: PropertyAccess): MathValue = MathNumber(BigDecimalMath.pi(calc.mathContext))
  }
  
  object Euler extends CalculatorSpecial.Dynamic("e") {
    override def value(calc: PropertyAccess): MathValue = MathNumber(BigDecimalMath.e(calc.mathContext))
  }
  
  object ImaginaryUnit extends CalculatorSpecial.Constant("i") {
    override def value: MathValue = MathNumber(BigComplex.I)
  }
}
