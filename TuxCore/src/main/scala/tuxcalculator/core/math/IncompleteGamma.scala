package tuxcalculator.core.math

import ch.obermuhlner.math.big.{BigComplex, BigComplexMath, BigDecimalMath}
import tuxcalculator.core.value.ValueHelper

import java.math.{MathContext, RoundingMode, BigDecimal => BigDec}

object IncompleteGamma {

  def gamma(p: BigComplex, x: BigComplex, mc: MathContext): BigComplex = {
    if (BigComplex.ZERO.equals(x)) return gammaC(p, mc)
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
}
