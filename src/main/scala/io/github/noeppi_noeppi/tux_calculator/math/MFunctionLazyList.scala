package io.github.noeppi_noeppi.tux_calculator.math

import io.github.noeppi_noeppi.tux_calculator.math.parser.FuncData

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class MFunctionLazyList(private val length: Int, private val list: Int => Double) extends MFunction{

  private val calculated = mutable.Set[Int]()
  private val cache = ListBuffer.fill(length)(Double.NaN)

  override val name: String = ""
  override val params: Int = 1
  override def result(functionPointers: FuncData, param: Double*): Double = {
    if (param(0) == -1) {
      length
    } else if (param(0) < 0 || param(0) >= length) {
      Double.NaN
    } else {
      if (!calculated.contains(param(0).toInt)) {
        cache(param(0).toInt) = list(param(0).toInt)
        calculated += param(0).toInt
      }
      cache(param(0).toInt)
    }
  }

  lazy val values: List[Double] = {
    for (i <- 0 until length) {
      if (!calculated.contains(i)) {
        cache(i) = list(i)
        calculated += i
      }
    }
    cache.toList
  }
  override val name2: String = "Lazy Liste"
}
