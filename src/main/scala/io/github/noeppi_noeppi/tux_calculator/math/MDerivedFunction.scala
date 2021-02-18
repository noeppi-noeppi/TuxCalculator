package io.github.noeppi_noeppi.tux_calculator.math

import io.github.noeppi_noeppi.tux_calculator.math.parser.FuncData

class MDerivedFunction(override val name: String, val func: MFunction) extends MFunction {

  override val params: Int = func.params
  override def result(functionPointers: FuncData, param: Double*): Double = func.result(functionPointers, param: _*)

  override val name2: String = func.name2
  override val doc: String = func.doc
}
