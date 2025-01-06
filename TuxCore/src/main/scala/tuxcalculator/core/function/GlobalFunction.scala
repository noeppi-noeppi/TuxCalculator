package tuxcalculator.core.function

import tuxcalculator.core.Calculator
import tuxcalculator.core.expression.Ast.Signature
import tuxcalculator.core.lexer.FmtCode
import tuxcalculator.core.value.{MathError, MathFunction, MathValue}

import scala.annotation.tailrec

class GlobalFunction(val name: String, val map: Map[Descriptor, MathFunction] = Map()) extends MathFunction {
  override def string(calc: Calculator): String = calc.format(FmtCode.Reference) + name
  
  def definitionCount: Int = map.size
  
  // Need to make a new function as all values are immutable
  def extend(sig: Signature, func: MathFunction): GlobalFunction = new GlobalFunction(name, map ++ ((sig.descriptor, func) :: Nil))
  
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = {
    map.get(Descriptor(args.length, vararg = false)) match {
      case Some(func) => func.applyTo(calc, args)
      case None =>
        @tailrec
        def tryApplyVararg(len: Int): MathValue = map.get(Descriptor(len, vararg = true)) match {
          case Some(func) => func.applyTo(calc, args)
          case None if len == 0 => MathError("Function '" + name + "' is not defined for " + args.length + " arguments.")
          case None => tryApplyVararg(len - 1)
        }
        tryApplyVararg(args.length + 1) // We can call at most vararg with one more parameter (where vararg gets an empty list)
    }
  }
}
