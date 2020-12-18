package io.github.noeppi_noeppi.tux_calculator.math

import io.github.noeppi_noeppi.tux_calculator.math.parser.FuncData

class MFunctionList(val values: Seq[Double]) extends MFunction {

  override val name: String = ""
  override val params: Int = 1

  override def result(functionPointers: FuncData, param: Double*): Double = {
    if (param(0) == -1) {
      values.size
    } else if (param(0) < 0 || param(0) >= values.size) {
      Double.NaN
    } else {
      values(param(0).toInt)
    }
  }

  override val name2: String = "Liste"
}