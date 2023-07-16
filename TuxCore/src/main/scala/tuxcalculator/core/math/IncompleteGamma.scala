package tuxcalculator.core.math

import ch.obermuhlner.math.big.{BigComplex, BigComplexMath, BigDecimalMath}
import tuxcalculator.core.value.ValueHelper

import java.math.{MathContext, RoundingMode, BigDecimal => BigDec}

object IncompleteGamma {
  
  def gamma(p: BigComplex, x: BigComplex, mc: MathContext): BigComplex = {
    if (BigComplex.ZERO.equals(x)) return gammaC(p, mc)
    if (BigComplex.ZERO.equals(p)) return gammaP0(x, mc)
    val theMc = new MathContext(mc.getPrecision << 1, RoundingMode.HALF_EVEN)
    val gammaP: BigComplex = gammaC(p, theMc)
    val sum = gammaSum(p, x, mc, theMc)
    val lGammaN = BigComplexMath.pow(x, p, theMc).multiply(BigComplexMath.exp(x.negate(), theMc)).multiply(sum, theMc)
    gammaP.subtract(lGammaN.multiply(gammaP, theMc), theMc).round(mc)
  }
  
  private def gammaSum(p: BigComplex, x: BigComplex, checkMc: MathContext, theMc: MathContext): BigComplex = {
    // gamma(p,x)/Gamma(p)
    var last: BigComplex = null
    var sum: BigComplex = BigComplex.ZERO
    var k: Int = 0
    while (last != sum.round(checkMc)) {
      last = sum.round(checkMc)
      val xk: BigComplex = BigComplexMath.pow(x, k, theMc)
      val gamma: BigComplex = gammaC(p.add(BigComplex.valueOf(k), theMc).add(BigComplex.ONE, theMc), theMc)
      sum = sum.add(xk.divide(gamma, theMc), theMc)
      k += 1
    }
    sum
  }

  private def gammaR(p: BigDec, mc: MathContext): BigDec = {
    val d: BigDecimal = p
    if (d.isWhole) {
      if (d <= 0) ValueHelper.plainError("Gamma(" + p + ") is undefined")
      else if (d.isValidInt) BigDecimalMath.factorial(d.toInt - 1).round(mc)
      else BigDecimalMath.gamma(p, mc)
    } else {
      BigDecimalMath.gamma(p, mc)
    }
  }
  
  private def gammaC(p: BigComplex, mc: MathContext): BigComplex = {
    if (p.isReal) {
      BigComplex.valueOf(gammaR(p.re, mc))
    } else {
      BigComplexMath.gamma(p, mc)
    }
  }
  
  private def gammaP0(x: BigComplex, mc: MathContext): BigComplex = {
    if (BigComplex.ZERO.equals(x)) ValueHelper.plainError("Gamma(" + x + ") is undefined")
    // Special case: Our usual approach does not work when p=0 as Gamma(0) and such Gamma(p) is undefined.
    // Use the exponential integral instead.
    val theMc = new MathContext(mc.getPrecision << 1, RoundingMode.HALF_EVEN)
    val neg: BigComplex = x.negate()
    val expInt: BigComplex = LogarithmicIntegral.logarithmicIntegral(BigComplexMath.exp(neg, theMc), mc) // regular mc
    println(BigComplexMath.exp(neg, theMc))
    println(expInt)
    val ln = BigComplexMath.log(x, theMc)
    val rLog: BigComplex = BigComplexMath.log(neg, theMc).subtract(BigComplexMath.log(BigComplexMath.reciprocal(neg, theMc), theMc), theMc)
    expInt.negate().add(rLog.divide(MathHelper.C_TWO, theMc)).subtract(ln)
  }
}
