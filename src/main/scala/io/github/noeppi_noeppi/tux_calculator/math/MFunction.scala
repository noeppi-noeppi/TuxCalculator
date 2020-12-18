package io.github.noeppi_noeppi.tux_calculator.math

import io.github.noeppi_noeppi.tux_calculator.math.parser.FuncData

trait MFunction extends DocumentationObject {
  val name: String
  val params: Int
  def result(functionPointers: FuncData, param: Double*): Double

  override final val dtype = DocType.FUNCTION
}