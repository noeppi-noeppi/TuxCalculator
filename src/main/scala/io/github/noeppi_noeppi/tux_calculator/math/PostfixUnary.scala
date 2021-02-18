package io.github.noeppi_noeppi.tux_calculator.math

import io.github.noeppi_noeppi.tux_calculator.math.parser.FuncData

trait PostfixUnary extends DocumentationObject {
  val name: String
  def apply(functionPointers: FuncData, op: Double): Double

  override final val dtype = DocType.POSTFIX
}
