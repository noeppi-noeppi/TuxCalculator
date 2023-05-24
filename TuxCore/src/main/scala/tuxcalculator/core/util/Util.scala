package tuxcalculator.core.util

import java.math.{BigDecimal => BigDec}

object Util {
  
  def makeString(codePoints: collection.Seq[Int]): String = codePoints.map(Character.toString).mkString
  def decomposeString(string: String): Vector[Int] = string.codePoints().toArray.toVector

  // stripTrailingZeros can overflow a non-overflowing number by decreasing the scale below Int.MinValue
  // which would cause an ArithmeticException.
  // This method will instead keep as many trailing zeros as required to not overflow the number.
  def safeStripTrailingZeros(num: BigDecimal): BigDec = safeStripTrailingZeros(num.bigDecimal)
  def safeStripTrailingZeros(num: BigDec): BigDec = try {
    num.stripTrailingZeros()
  } catch {
    case _: ArithmeticException => num.setScale(Int.MinValue)
  }
}
