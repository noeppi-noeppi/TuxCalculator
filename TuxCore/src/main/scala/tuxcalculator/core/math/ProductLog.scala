package tuxcalculator.core.math

import ch.obermuhlner.math.big.{BigComplex, BigComplexMath, BigDecimalMath}

import java.math.{MathContext, RoundingMode, BigDecimal => BigDec}

object ProductLog {

  private val R_TWO = BigDec.valueOf(2)
  private val C_TWO = BigComplex.valueOf(2)
  private val REAl_SEP = BigDec.valueOf(-0.3678794411716)
  
  def productLog(value: BigComplex, mc: MathContext): BigComplex = {
    if (BigComplex.ZERO.equals(value)) return BigComplex.ZERO
    
    // Calculate on higher precision and compare against requested
    val theMc = new MathContext(mc.getPrecision << 1, RoundingMode.HALF_EVEN)
    val initialGuess = if (value.re.compareTo(BigDec.TEN) < 0) BigComplexMath.log(value.subtract(REAl_SEP, theMc), theMc) else {
      val theLn = BigComplexMath.log(value, theMc)
      val theLnLn = BigComplexMath.log(theLn, theMc)
      theLn.add(theLnLn.multiply(theLn.reciprocal(theMc).subtract(BigComplex.ONE, theMc), theMc), theMc)
    }
    
    val maxItr = Math.max(10, mc.getPrecision / 2)
    val discard = -2000 * mc.getPrecision

    if (value.isReal && value.re.compareTo(REAl_SEP) > 0) {
      // Special case: We have a real result, calculate entirely on real numbers
      var guess: BigDec = initialGuess.re
      
      for (_ <- 0 until maxItr) {
        val expGuess = BigDecimalMath.exp(guess, theMc)
        val wGuess = guess.multiply(expGuess, theMc)
        val numer = wGuess.subtract(value.re, theMc)
        val incrWGuess = guess.add(BigDec.ONE, theMc).multiply(expGuess, theMc)
        val subPartNum = guess.add(R_TWO, theMc).multiply(numer, theMc)
        val subPartDenom = R_TWO.multiply(guess, theMc).add(R_TWO, theMc)
        val subPart = subPartNum.divide(subPartDenom, theMc)
        val denom = incrWGuess.subtract(subPart, theMc)
        guess = guess.subtract(numer.divide(denom, theMc), theMc)

        if (guess.scale() < discard) guess = BigDec.ZERO

        if (guess.multiply(BigDecimalMath.exp(guess, theMc), theMc).round(mc).compareTo(value.re) == 0) {
          return BigComplex.valueOf(guess.round(mc))
        }
      }
      BigComplex.valueOf(guess.round(mc))
    } else {
      var guess: BigComplex = initialGuess

      for (_ <- 0 until maxItr) {
        val expGuess = BigComplexMath.exp(guess, theMc)
        val wGuess = guess.multiply(expGuess, theMc)
        val numer = wGuess.subtract(value, theMc)
        val incrWGuess = guess.add(BigComplex.ONE, theMc).multiply(expGuess, theMc)
        val subPartNum = guess.add(C_TWO, theMc).multiply(numer, theMc)
        val subPartDenom = C_TWO.multiply(guess, theMc).add(C_TWO, theMc)
        val subPart = subPartNum.divide(subPartDenom, theMc)
        val denom = incrWGuess.subtract(subPart, theMc)
        guess = guess.subtract(numer.divide(denom, theMc), theMc)

        if (guess.re.scale() < discard) guess = guess.im()
        if (guess.im.scale() < discard) guess = guess.re()

        if (guess.multiply(BigComplexMath.exp(guess, theMc), theMc).round(mc).equals(value)) {
          return guess.round(mc)
        }
      }
      guess.round(mc)
    }
  }
}
