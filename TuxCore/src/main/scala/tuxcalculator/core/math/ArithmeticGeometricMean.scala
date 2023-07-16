package tuxcalculator.core.math

import ch.obermuhlner.math.big.{BigComplex, BigDecimalMath}

import java.math.{MathContext, RoundingMode, BigDecimal => BigDec}

object ArithmeticGeometricMean {

  def agm(gm: BigComplex, am: BigComplex, mc: MathContext): BigComplex = {
    if (gm.isReal && gm.re.compareTo(BigDec.ZERO) > 0 && am.isReal && am.re.compareTo(BigDec.ZERO) > 0) {
      BigComplex.valueOf(agmR(gm.re, am.re, mc))
    } else {
      agmC(gm, am, mc)
    }
  }
  
  // Both numbers must be positive
  private def agmR(gm: BigDec, am: BigDec, mc: MathContext): BigDec = {
    val theMc = new MathContext(mc.getPrecision + 4, RoundingMode.HALF_EVEN)
    var theGm: BigDec = gm
    var theAm: BigDec = am
    while (theGm.round(mc).compareTo(theAm.round(mc)) != 0) {
      (BigDecimalMath.sqrt(theGm.multiply(theAm, theMc), theMc), theGm.add(theAm, theMc).divide(MathHelper.R_TWO, theMc)) match {
        case (g, a) =>
          theGm = g
          theAm = a
      }
    }
    theGm
  }

  private def agmC(gm: BigComplex, am: BigComplex, mc: MathContext): BigComplex = {
    val theMc = new MathContext(mc.getPrecision << 1, RoundingMode.HALF_EVEN)
    var theGm: BigComplex = gm
    var theAm: BigComplex = am
    while (theGm.round(mc) != theAm.round(mc)) {
      (MathHelper.complexSqrt(theGm.multiply(theAm, theMc), theMc), theGm.add(theAm, theMc).divide(MathHelper.C_TWO, theMc)) match {
        case (g, a) =>
          theGm = g
          theAm = a
      }
    }
    theGm
  }
}
