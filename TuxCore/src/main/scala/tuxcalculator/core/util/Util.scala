package tuxcalculator.core.util

object Util {
  
  def makeString(codePoints: collection.Seq[Int]): String = codePoints.map(Character.toString).mkString
  def decomposeString(string: String): Vector[Int] = string.codePoints().toArray.toVector
}
