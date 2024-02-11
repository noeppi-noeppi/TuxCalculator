package tuxcalculator.core.util

import java.io.{PrintWriter, StringWriter}
import java.lang.{StringBuilder => JStringBuilder}
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
  
  // Always use scientific notation with a single digit before the decimal separator.
  // Required to make number formatting consistent
  def formatScientific(num: BigDec): String = {
    if (num.compareTo(BigDec.ZERO) == 0) return "0e0"
    val digits: Array[Char] = num.unscaledValue().abs().toString.toCharArray
    val sb: JStringBuilder = new JStringBuilder(15 + digits.length)
    val exp: Long = -num.scale().toLong + digits.length - 1
    if (num.signum() < 0) sb.append("-")
    sb.append(digits, 0, 1)
    if (digits.length > 1) {
      sb.append('.')
      sb.append(digits, 1, digits.length - 1)
    }
    sb.append("e")
    sb.append(exp)
    sb.toString
  }
  
  def getStacktrace(t: Throwable): Vector[String] = {
    val writer = new StringWriter()
    val printer = new PrintWriter(writer)
    t.printStackTrace(printer)
    printer.close()
    writer.toString.split("\n").toVector.filter(_.nonEmpty)
  }

  private val SuperscriptCharacters: Map[Char, Char] = Map(
    '-' -> '⁻', '0' -> '⁰', '1' -> '¹', '2' -> '²',
    '3' -> '³', '4' -> '⁴', '5' -> '⁵', '6' -> '⁶',
    '7' -> '⁷', '8' -> '⁸', '9' -> '⁹'
  )
  def toSuperscript(value: Int): String = value.toString.map(chr => SuperscriptCharacters.getOrElse(chr, chr))
}
