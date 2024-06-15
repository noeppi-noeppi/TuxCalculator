package tuxcalculator.core.special

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.value.{MathBoolean, MathError, MathNumber, MathRealNumeric, MathValue}

object LogicOperators {
  
  abstract class NumericBooleanOp(name: String, boolOp: (Boolean, Boolean) => Boolean, intOp: (BigInt, BigInt) => BigInt) extends CalculatorSpecial.SimpleFunction(name, 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = (args(0), args(1)) match {
      case (MathBoolean(b1), MathBoolean(b2)) => MathBoolean(boolOp(b1, b2))
      case (MathBoolean(b1), MathRealNumeric(n2)) if n2.isWhole => MathNumber(BigDecimal(intOp(boolToInt(b1, n2.toBigInt), n2.toBigInt)))
      case (MathRealNumeric(n1), MathBoolean(b2)) if n1.isWhole => MathNumber(BigDecimal(intOp(n1.toBigInt, boolToInt(b2, n1.toBigInt))))
      case (MathRealNumeric(n1), MathRealNumeric(n2)) if n1.isWhole && n2.isWhole => MathNumber(BigDecimal(intOp(n1.toBigInt, n2.toBigInt)))
      case (v1, v2)=> MathError("Can't " + name + " " + calc.format(v1) + " and " + calc.format(v2))
    }
    
    private def boolToInt(value: Boolean, other: BigInt): BigInt = {
      if (!value) 0
      else BigInt(2).pow(other.abs.bitLength `max` 1) - 1
    }
  }
  
  object And extends NumericBooleanOp("and", _ && _, _ & _)
  object Or extends NumericBooleanOp("or", _ || _, _ | _)
  object Xor extends NumericBooleanOp("xor", _ != _, _ ^ _)
}
