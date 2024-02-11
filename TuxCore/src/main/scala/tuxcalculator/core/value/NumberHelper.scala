package tuxcalculator.core.value

import ch.obermuhlner.math.big.{BigComplex, BigComplexMath, BigDecimalMath}
import tuxcalculator.core.value.ValueHelper._

import java.math.RoundingMode

object NumberHelper {

  def add(v1: MathValue, v2: MathValue): MathValue = (get(v1), get(v2)) match {
    case (MathRealNumeric(n1), MathRealNumeric(n2)) => MathNumber(n1.bigDecimal.add(n2.bigDecimal, calc.mathContext))
    case (MathNumeric(n1), MathNumeric(n2)) => MathNumber(n1.add(n2, calc.mathContext))
    case (MathPolynomic(p1), MathPolynomic(p2)) => PolynomialOps.add(calc, p1, p2)
    case (m1: MathMatrix, m2: MathMatrix) => MatrixOps.join(m1, m2, add)
    case _ => MathError("Can't add " + calc.format(v1) + " and " + calc.format(v2))
  }

  def sub(v1: MathValue, v2: MathValue): MathValue = (get(v1), get(v2)) match {
    case (MathRealNumeric(n1), MathRealNumeric(n2)) => MathNumber(n1.bigDecimal.subtract(n2.bigDecimal, calc.mathContext))
    case (MathNumeric(n1), MathNumeric(n2)) => MathNumber(n1.subtract(n2, calc.mathContext))
    case (MathPolynomic(p1), MathPolynomic(p2)) => PolynomialOps.sub(calc, p1, p2)
    case (m1: MathMatrix, m2: MathMatrix) => MatrixOps.join(m1, m2, sub)
    case _ => MathError("Can't sub " + calc.format(v1) + " and " + calc.format(v2))
  }
  
  def mul(v1: MathValue, v2: MathValue): MathValue = (get(v1), get(v2)) match {
    case (MathRealNumeric(n1), MathRealNumeric(n2)) => MathNumber(n1.bigDecimal.multiply(n2.bigDecimal, calc.mathContext))
    case (MathNumeric(n1), MathNumeric(n2)) => MathNumber(n1.multiply(n2, calc.mathContext))
    case (MathPolynomic(p1), MathPolynomic(p2)) => PolynomialOps.mul(calc, p1, p2)
    case (mat: MathMatrix, num @ MathNumeric(_)) => MatrixOps.transform(mat, entry => mul(entry.value, num))
    case (num @ MathNumeric(_), mat: MathMatrix) => MatrixOps.transform(mat, entry => mul(entry.value, num))
    case (m1: MathMatrix, m2: MathMatrix) => MatrixOps.mul(m1, m2)
    case _ => MathError("Can't mul " + calc.format(v1) + " and " + calc.format(v2))
  }
  
  def div(v1: MathValue, v2: MathValue): MathValue = (get(v1), get(v2)) match {
    case (_, MathNumeric(n2)) if n2 == BigComplex.ZERO => MathError("Division by zero")
    case (MathRealNumeric(n1), MathRealNumeric(n2)) => MathNumber(n1.bigDecimal.divide(n2.bigDecimal, calc.mathContext))
    case (MathNumeric(n1), MathRealNumeric(n2)) => MathNumber(n1.divide(n2.bigDecimal, calc.mathContext))
    case (MathNumeric(n1), MathNumeric(n2)) => MathNumber(n1.divide(n2, calc.mathContext))
    case (MathPolynomic(p1), MathPolynomic(p2)) => PolynomialOps.div(calc, p1, p2)
    case (mat: MathMatrix, num @ MathNumeric(_)) => MatrixOps.transform(mat, entry => div(entry.value, num))
    case (m1: MathMatrix, m2: MathMatrix) => mul(m1, MatrixOps.invert(m2))
    case _ => MathError("Can't div " + calc.format(v1) + " and " + calc.format(v2))
  }
  
  def truncatingDiv(v1: MathValue, v2: MathValue): MathValue = {
    import java.math.{BigDecimal => BigDec}
    def truncateR(num: BigDec): BigDec = num.setScale(0, RoundingMode.DOWN)
    def truncateC(num: BigComplex): BigComplex = BigComplex.valueOf(truncateR(num.re), truncateR(num.im))
    (get(v1), get(v2)) match {
      case (_, MathNumeric(n2)) if n2 == BigComplex.ZERO => MathError("Division by zero")
      case (MathRealNumeric(n1), MathRealNumeric(n2)) => MathNumber(truncateR(n1.bigDecimal.divide(n2.bigDecimal, calc.mathContext)))
      case (MathNumeric(n1), MathRealNumeric(n2)) => MathNumber(truncateC(n1.divide(n2.bigDecimal, calc.mathContext)))
      case (MathNumeric(n1), MathNumeric(n2)) => MathNumber(truncateC(n1.divide(n2, calc.mathContext)))
      case (MathPolynomic(p1), MathPolynomic(p2)) => PolynomialOps.truncatingDiv(calc, p1, p2)
      case (mat: MathMatrix, num @ MathNumeric(_)) => MatrixOps.transform(mat, entry => div(entry.value, num))
      case (m1: MathMatrix, m2: MathMatrix) => mul(m1, MatrixOps.invert(m2))
      case _ => MathError("Can't div " + calc.format(v1) + " and " + calc.format(v2))
    }
  }
  
  def mod(v1: MathValue, v2: MathValue): MathValue = (get(v1), get(v2)) match {
    case (_, MathNumeric(n2)) if n2 == BigComplex.ZERO => MathError("Division by zero")
    case (MathRealNumeric(n1), MathRealNumeric(n2)) => MathNumber(n1.bigDecimal.remainder(n2.bigDecimal, calc.mathContext))
    case (MathNumeric(_), MathNumeric(_)) => MathError("Can't modulo complex numbers.")
    case (MathPolynomic(p1), MathPolynomic(p2)) => PolynomialOps.mod(calc, p1, p2)
    case _ => MathError("Can't modulo " + calc.format(v1) + " and " + calc.format(v2))
  }
  
  def pow(v1: MathValue, v2: MathValue): MathValue = (get(v1), get(v2)) match {
    case (MathNumeric(n1), MathNumeric(n2)) if n1 == BigComplex.ZERO && n2 == BigComplex.ZERO => MathError("0^0 is undefined")
    case (MathNumeric(n1), MathNumeric(n2)) if n1 == BigComplex.ZERO && BigDecimal(n2.re) < 0 => MathError("Division by zero")
    case (MathNumeric(n1), MathNumeric(_)) if n1 == BigComplex.ZERO => MathNumber.Zero
    case (MathRealNumeric(n1), MathRealNumeric(n2)) if n1 > 0 || n2.isValidLong => MathNumber(BigDecimalMath.pow(n1.bigDecimal, n2.bigDecimal, calc.mathContext))
    case (MathNumeric(n1), MathNumeric(n2)) if n2.isReal => MathNumber(BigComplexMath.pow(n1, n2.re, calc.mathContext))
    case (MathNumeric(n1), MathNumeric(n2)) => MathNumber(BigComplexMath.pow(n1, n2, calc.mathContext))
    case (MathPolynomic(p1), MathRealNumeric(n2)) if n2.isWhole => PolynomialOps.raise(calc, p1, realInt(v2).toInt)
    case (mat: MathMatrix, MathRealNumeric(n2)) if n2.isWhole => MatrixOps.raise(mat, realInt(v2).toInt)
    case _ => MathError("Can't pow " + calc.format(v1) + " and " + calc.format(v2))
  }
  
  def addIdentity(value: MathValue): MathValue = get(value) match {
    case MathNumeric(_) | MathPolynomic(_) => MathNumber.Zero
    case mat: MathMatrix => MatrixOps.zero(mat.width, mat.height)
    case _ => MathError("Can't compute sums of " + calc.format(value))
  }
  
  def mulIdentity(value: MathValue): MathValue = get(value) match {
    case MathNumeric(_) | MathPolynomic(_) => MathNumber.One
    case mat: MathMatrix if mat.width == mat.height => MatrixOps.identity(mat.width)
    case _ => MathError("Can't compute powers of " + calc.format(value))
  }
}
