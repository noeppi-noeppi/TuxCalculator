package io.github.noeppi_noeppi.tux_calculator.math

trait PostfixUnary extends DocumentationObject {
  val name: String
  def apply(op: Double): Double

  override final val dtype = DocType.POSTFIX
}
