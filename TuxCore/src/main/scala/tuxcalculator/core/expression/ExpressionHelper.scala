package tuxcalculator.core.expression

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorProperties
import tuxcalculator.core.function.LambdaFunction
import tuxcalculator.core.resolution.BindLogic

object ExpressionHelper {
  
  def makeLambdaLike(calc: Calculator, sig: Ast.Signature, expr: Ast.Expression): LambdaFunction = {
    val boundExpr = if (calc.properties(CalculatorProperties.Eager)) {
      BindLogic.bind(expr, calc, eager = true, freeVars = sig.names.toSet).bound
    } else {
      expr
    }
    new LambdaFunction(sig, boundExpr, expr)
  }
}
