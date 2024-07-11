package tuxcalculator.core.special

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.value._

object TestOperators {
  
  object IsList extends CalculatorSpecial.SimpleFunction("isl", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = MathBoolean(args.head.isInstanceOf[MathList])
  }
  
  object IsMatrix extends CalculatorSpecial.SimpleFunction("ism", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = MathBoolean(args.head.isInstanceOf[MathMatrix])
  }
  
  object IsVector extends CalculatorSpecial.SimpleFunction("isv", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = args.head match {
      case MathVector(_) => MathTrue
      case _ => MathFalse
    }
  }
  
  object IsBoolean extends CalculatorSpecial.SimpleFunction("isb", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = MathBoolean(args.head == MathTrue || args.head == MathFalse)
  }
  
  object IsNumber extends CalculatorSpecial.SimpleFunction("isn", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = args.head match {
      case MathNumeric(_) => MathTrue
      case _ => MathFalse
    }
  }
  
  object IsReal extends CalculatorSpecial.SimpleFunction("isr", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = args.head match {
      case MathRealNumeric(_) => MathTrue
      case _ => MathFalse
    }
  }
  
  object IsPolynomial extends CalculatorSpecial.SimpleFunction("isp", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = args.head match {
      case MathPolynomic(_) => MathTrue
      case _ => MathFalse
    }
  }
}
