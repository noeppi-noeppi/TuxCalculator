package tuxcalculator.core.special

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.value.{MathError, MathMatrix, MathNumber, MathValue, MatrixOps, ValueHelper}

object MatrixOperators {

  object Wd extends CalculatorSpecial.SimpleFunction("wd", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      ValueHelper.get(args.head) match {
        case m@MathMatrix(_) => MathNumber(m.width)
        case _ => MathError("Can't get width of: " + calc.format(args.head))
      }
    }
  }

  object Ht extends CalculatorSpecial.SimpleFunction("ht", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      ValueHelper.get(args.head) match {
        case m@MathMatrix(_) => MathNumber(m.height)
        case _ => MathError("Can't get height of: " + calc.format(args.head))
      }
    }
  }
  
  object Adj extends CalculatorSpecial.SimpleFunction("adj", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      val mat = ValueHelper.matrix(args.head)
      MatrixOps.adj(mat)
    }
  }
  
  object Det extends CalculatorSpecial.SimpleFunction("det", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      val mat = ValueHelper.matrix(args.head)
      MatrixOps.det(mat)
    }
  }
}
