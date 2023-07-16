package tuxcalculator.core.math

import ch.obermuhlner.math.big.{BigComplex, BigComplexMath, BigDecimalMath}

import java.math.{MathContext, BigDecimal => BigDec}

object MathHelper {

  val R_TWO: BigDec = BigDec.valueOf(2)
  val C_TWO: BigComplex = BigComplex.valueOf(2)
  
  def complexSqrt(x: BigComplex, mc: MathContext): BigComplex = {
    // Workaround for https://github.com/eobermuhlner/big-math/issues/66
    x match {
      case x if x.im.compareTo(BigDec.ZERO) == 0 && x.re.compareTo(BigDec.ZERO) <= 0 => BigComplex.I.multiply(BigDecimalMath.sqrt(x.re.negate(), mc))
      case x => BigComplexMath.sqrt(x, mc)
    }
  }
}
