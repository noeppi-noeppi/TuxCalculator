package io.github.noeppi_noeppi.tux_calculator.gui

import io.github.noeppi_noeppi.tux_calculator.math.parser.Parser

trait GuiType {

  def display(parser: Parser): Unit
}
