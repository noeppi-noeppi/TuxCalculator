package tuxcalculator.core.special

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.function.{MemoizedFunction, SelfReferenceFunction}
import tuxcalculator.core.value.{MathFunction, MathValue, ValueHelper}

object FunctionOperators {

  object Y extends CalculatorSpecial.SimpleFunction("y", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      new SelfReferenceFunction(args(0))
    }
  }
  
  object Memoize extends CalculatorSpecial.SimpleFunction("memoize", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      args(0) match {
        case function: MemoizedFunction => function
        case function: MathFunction => new MemoizedFunction(function)
        case value => value
      }
    }
  }
}
