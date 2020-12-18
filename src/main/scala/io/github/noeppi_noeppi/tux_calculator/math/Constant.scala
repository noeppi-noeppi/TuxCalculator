package io.github.noeppi_noeppi.tux_calculator.math

trait Constant extends DocumentationObject {
  val name: String
  val value: Double

  override final val dtype = DocType.CONSTANT
}
