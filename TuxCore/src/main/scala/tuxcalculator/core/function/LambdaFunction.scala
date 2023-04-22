package tuxcalculator.core.function

import tuxcalculator.core.Calculator
import tuxcalculator.core.expression.Ast
import tuxcalculator.core.expression.Ast.Signature
import tuxcalculator.core.resolution.{BindLogic, ComputationLogic}
import tuxcalculator.core.value.{MathError, MathFunction, MathList, MathValue}

class LambdaFunction(val sig: Ast.Signature, val code: Ast.Expression, val definitionCode: Ast.Expression) extends MathFunction {
  override def string(calc: Calculator): String = "\\" + sig + " -> " + definitionCode.string(calc)
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = {
    val argValues: Map[String, MathValue] = LambdaFunction.makeArgValueMap(sig, args) match {
      case Some(map) => map
      case None => return MathError("Function is not defined for " + args.length + " arguments: " + calc.format(this))
    }
    // Replace definition code (used for toString) of nested lambdas as we eagerly bind through them.
    val boundCode = BindLogic.bind(code, calc, eager = true, specialValues = argValues)
    ComputationLogic.compute(boundCode, calc)
  }
}

object LambdaFunction {
  def makeArgValues(sig: Signature, args: Vector[MathValue]): Option[Vector[MathValue]] = sig match {
    case Ast.Signature(names, false) if names.length == args.length => Some(args)
    case Ast.Signature(names, true) if names.nonEmpty && names.length - 1 <= args.length =>
      val regularArgs: Vector[MathValue] = args.take(names.length - 1)
      val varargValue: MathValue = MathList(args.drop(names.length - 1))
      Some(regularArgs ++ Vector(varargValue))
    case _ => None
  }


  def makeArgValueMap(sig: Signature, args: Vector[MathValue]): Option[Map[String, MathValue]] = makeArgValues(sig, args) match {
    case Some(argValues) => Some((sig.names zip argValues).toMap)
    case None => None
  }
}
