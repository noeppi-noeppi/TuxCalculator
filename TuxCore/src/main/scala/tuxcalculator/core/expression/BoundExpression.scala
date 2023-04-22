package tuxcalculator.core.expression

import tuxcalculator.core.Calculator

class BoundExpression(val bound: Ast.Expression, val raw: Ast.Expression, val unboundErrors: Set[String] = Set()) {
  def string(calc: Calculator): String = raw.string(calc)
}
