package tuxcalculator.core.resolution

import tuxcalculator.core.Calculator
import tuxcalculator.core.expression.{Ast, BoundExpression}
import tuxcalculator.core.function.{LambdaFunction, MatchFunction, MatchFunctionEntry, PartialAppliedFunction}
import tuxcalculator.core.value.{MathError, MathList, MathMatrix, MathValue, MathVoid}

object ComputationLogic {

  def compute(expr: BoundExpression, calc: Calculator): MathValue = {
    def processPartialArg(arg: Ast.PartialArgument): Seq[Option[MathValue]] = arg match {
      case Ast.Placeholder => Seq(None)
      case a: Ast.Argument => processArg(a).map(Some(_))
    }
    
    def processArg(arg: Ast.Argument): Seq[MathValue] = arg match {
      case Ast.SplattedArgument(listExpr) => normalize(process(listExpr)) match {
        case MathList(values) => values
        case MathMatrix(values) if values.length == 1 => values.head
        case res: MathError => res :: Nil
        case res => MathError("Can't splat value: '" + calc.format(res) + "'") :: Nil
      }
      case theExpr: Ast.Expression => process(theExpr) :: Nil
    }
    
    def process(expr: Ast.Expression): MathValue = expr match {
      case Ast.Group(nested) => process(nested)
      case Ast.Value(value) => value
      case Ast.List(args) => MathList(args.flatMap(processArg))
      case Ast.Matrix(args) if args.nonEmpty && args.map(col => col.length).distinct.size != 1 => MathError("Matrix literal with different sized columns")
      case Ast.Matrix(args) if args.isEmpty || args.head.isEmpty => MathError("Empty matrix")
      case Ast.Matrix(args) => MathMatrix(args.map(col => col.map(process)))
      case Ast.Lambda(sig, code, definitionCode) => new LambdaFunction(sig, code, definitionCode)
      case Ast.Match(entries) if entries.isEmpty => MathError("Empty match")
      case ast @ Ast.Match(entries) => new MatchFunction(entries.map {
        case Ast.MatchEntry(sig, elementGuards, mainGuard, code, definitionCode) => new MatchFunctionEntry(
          sig,
          elementGuards.map(eg => eg.map(expr => new LambdaFunction(Ast.Signature.Empty, expr.code, expr.definitionCode))),
          mainGuard.map(mg => new LambdaFunction(sig, mg.code, mg.definitionCode)),
          new LambdaFunction(sig, code, definitionCode)
        )
      }, ast)
      case Ast.Application(value, args) => doApply(process(value), args.flatMap(processPartialArg), forcePartial = false)
      case Ast.PartialApplication(value, args) => doApply(process(value), args.flatMap(processPartialArg), forcePartial = true)
      case elem => MathError("Computing an unbound expression: '" + elem.string(calc) + "' This is a bug.")
    }
    
    def doApply(value: MathValue, args: Vector[Option[MathValue]], forcePartial: Boolean): MathValue = {
      def argString(arg: Option[MathValue]): String = arg match {
        case Some(value) => calc.format(value)
        case None => "_"
      }
      
      val normArgs: Vector[Option[MathValue]] = args.map(_.map(normalize))
      def resultOrVoid(result: => MathValue): MathValue = if (normArgs.contains(Some(MathVoid))) MathVoid else result
      val partial = forcePartial || normArgs.contains(None)
      
      if (value == MathVoid) MathVoid else value match {
        case err: MathError if partial => err.trace("Partially applied to " + args.map(argString).mkString("(", ", ", ")"))
        case err: MathError => err.trace("Applied to " + args.map(argString).mkString("(", ", ", ")"))
        case _ => normArgs.flatMap[MathError] {
          case Some(err: MathError) => Some(err)
          case _ => None
        }.headOption match {
          case Some(err) if partial => err.trace("Passed as partial argument to " + calc.format(value))
          case Some(err) => err.trace("Passed as argument to " + calc.format(value))
          case None if partial && !normArgs.exists(_.isDefined) => resultOrVoid(value)
          case None if partial => resultOrVoid(PartialAppliedFunction.create(value, normArgs))
          case None => resultOrVoid(value.applyTo(calc, normArgs.map(_.get)))
        }
      }
    }
    
    def normalize(value: MathValue): MathValue = value match {
      case MathList(elems) if elems.contains(MathVoid) => MathVoid
      case MathMatrix(elems) if elems.exists(_.contains(MathVoid)) => MathVoid
      case MathList(elems) => elems.zipWithIndex.flatMap[(MathError, Int)] {
        case (err: MathError, idx) => Some((err, idx))
        case _ => None
      }.headOption match {
        case Some((err, idx)) => err.trace("Appeared in a list at index " + idx)
        case None => value
      }
      case MathMatrix(elems) => elems.zipWithIndex.flatMap[(MathError, Int, Int)](entry => {
        val (col, colIdx) = entry
        col.zipWithIndex.flatMap[(MathError, Int, Int)] {
          case (err: MathError, rowIdx) => Some((err, rowIdx, colIdx))
          case _ => None
        }
      }).headOption match {
        case Some((err, rowIdx, colIdx)) => err.trace("Appeared in a matrix at index " + rowIdx + ":" + colIdx)
        case None => value
      }
      case _ => value
    }
    
    normalize(process(expr.bound))
  }
}
