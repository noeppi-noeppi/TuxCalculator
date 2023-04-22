package tuxcalculator.core.special

import ch.obermuhlner.math.big.BigComplex
import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.value._

object BasicOperators {

  object Re extends CalculatorSpecial.SimpleFunction("re", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args.head) match {
      case MathComplexNumeric(real, _) => MathNumber(real)
      case _ => MathNumber.Zero
    }}
  }
  
  object Im extends CalculatorSpecial.SimpleFunction("im", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args.head) match {
      case MathComplexNumeric(_, imag) => MathNumber(imag)
      case _ => MathNumber.Zero
    }}
  }
  
  object Add extends CalculatorSpecial.SimpleFunction("add", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      NumberHelper.add(args(0), args(1))
    }
  }

  object Sub extends CalculatorSpecial.SimpleFunction("sub", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      NumberHelper.sub(args(0), args(1))
    }
  }

  object Mul extends CalculatorSpecial.SimpleFunction("mul", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      NumberHelper.mul(args(0), args(1))
    }
  }

  object Div extends CalculatorSpecial.SimpleFunction("div", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      NumberHelper.div(args(0), args(1))
    }
  }
  
  object Mod extends CalculatorSpecial.SimpleFunction("mod", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      NumberHelper.mod(args(0), args(1))
    }
  }

  object Pow extends CalculatorSpecial.SimpleFunction("pow", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      NumberHelper.pow(args(0), args(1))
    }
  }
  
  object Polar extends CalculatorSpecial.SimpleFunction("polar", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      (ValueHelper.get(args(0)), ValueHelper.get(args(1))) match {
        case (MathRealNumeric(n1), MathRealNumeric(n2)) => MathNumber(BigComplex.valueOfPolar(n1.bigDecimal, n2.bigDecimal, ValueHelper.calc.mathContext))
        case _ => MathError("Expected two real number to create polar complex")
      }
    }
  }
}
