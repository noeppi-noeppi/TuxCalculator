package io.github.noeppi_noeppi.tux_calculator.math

import io.github.noeppi_noeppi.tux_calculator.math.parser.FuncData

trait Operator extends DocumentationObject {
  val name: String
  val priority: Priority
  val rightAssoc: Boolean
  def apply(functionPointers: FuncData, op1: Double, op2: Double): Double
  

  override final val dtype = DocType.OPERATOR
}
