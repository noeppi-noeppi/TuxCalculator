package tuxcalculator.core.resolution

import tuxcalculator.core.Calculator
import tuxcalculator.core.expression.{Ast, BoundExpression}
import tuxcalculator.core.value.{MathError, MathValue}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object BindLogic {
  
  // Bind things that are still unbound
  def rebind(expr: BoundExpression, calc: Calculator, eager: Boolean = false, freeVars: Set[String] = Set(), specialValues: Map[String, MathValue] = Map()): BoundExpression = {
    val newBound = bind(expr.bound, calc, eager, freeVars, specialValues)
    new BoundExpression(newBound.bound, expr.raw, expr.unboundErrors | newBound.unboundErrors)
  }
  
  private def bindDef(expr: Ast.DefExpression, calc: Calculator, eager: Boolean = false, freeVars: Set[String] = Set(), specialValues: Map[String, MathValue] = Map()): BoundDefExpression = {
    val bound: BoundExpression = bind(expr.code, calc, eager, freeVars, specialValues)
    BoundDefExpression(bound, expr.definitionCode)
  }
  
  def bind(expr: Ast.Expression, calc: Calculator, eager: Boolean = false, freeVars: Set[String] = Set(), specialValues: Map[String, MathValue] = Map()): BoundExpression = {
    val errors: mutable.Set[String] = mutable.Set()
    
    def checkError(name: String, result: MathValue): MathValue = result match {
      case res @ MathError(_, _) => errors.add(name); res
      case res => res
    }
    
    def variable(name: String): MathValue = specialValues.get(name) match {
      case Some(value) => value
      case None => calc.resolution.variable(name)
    }
    
    def invocation(name: String): MathValue = calc.resolution.maybeGlobalFunction(name) match {
      case Some(value) => value
      case None => variable(name)
    }
    
    def processPartialArg(elem: Ast.PartialArgument): Ast.PartialArgument = elem match {
      case Ast.Placeholder => Ast.Placeholder
      case a: Ast.Argument => processArg(a)
    }
    
    def processArg(elem: Ast.Argument): Ast.Argument = elem match {
      case ex: Ast.Expression => process(ex)
      case Ast.SplattedArgument(ex) => Ast.SplattedArgument(process(ex))
    }
    
    def process(elem: Ast.Expression): Ast.Expression = elem match {
      case Ast.Group(nested) => process(nested)
      case Ast.Answer => Ast.Value(calc.answer)
      case Ast.Variable(name) if !freeVars.contains(name) => Ast.Value(checkError(name, variable(name)))
      case Ast.Reference(target) => Ast.Value(checkError(target.name, calc.resolution.reference(target)))
      case Ast.Special(name) => Ast.Value(checkError("#" + name, calc.specials(name)))
      case Ast.List(args) => Ast.List(args.map(processArg))
      case Ast.Matrix(args) => Ast.Matrix(args.map(col => col.map(process)))
      case Ast.Lambda(sig, code, defCode) if eager =>
        val boundCode = bind(code, calc, eager, freeVars | sig.names.toSet, specialValues)
        errors.addAll(boundCode.unboundErrors)
        Ast.Lambda(sig, boundCode.bound, defCode)
      case Ast.Match(entries) if eager => Ast.Match(entries.map {
        case Ast.MatchEntry(sig, elementGuards, mainGuard, code, defCode) =>
          val boundElementGuards = elementGuards.map(eg => eg.map(expr => bindDef(expr, calc, eager, freeVars, specialValues)))
          val boundMainGuard = mainGuard.map(expr => bindDef(expr, calc, eager, freeVars | sig.names.toSet, specialValues))
          val boundCode = bind(code, calc, eager, freeVars | sig.names.toSet, specialValues)
          errors.addAll(boundElementGuards.flatMap(eg => eg.toList.flatMap(_.code.unboundErrors)))
          errors.addAll(boundMainGuard.toList.flatMap(_.code.unboundErrors))
          errors.addAll(boundCode.unboundErrors)
          Ast.MatchEntry(sig, boundElementGuards.map(eg => eg.map(_.fullCode)), boundMainGuard.map(_.fullCode), boundCode.bound, defCode)
      })
      case Ast.Invocation(name, args) if freeVars.contains(name) => calc.resolution.maybeGlobalFunction(name) match {
        case Some(global) => Ast.Application(Ast.Value(global), args.map(processPartialArg))
        // A global function does not exist now, but we have a free variable
        // make sure, it will never bind to a global function
        case None => Ast.Application(Ast.Variable(name), args.map(processPartialArg))
      }
      case Ast.PartialInvocation(name, args) if freeVars.contains(name) => calc.resolution.maybeGlobalFunction(name) match {
        case Some(global) => Ast.PartialApplication(Ast.Value(global), args.map(processPartialArg))
        // A global function does not exist now, but we have a free variable
        // make sure, it will never bind to a global function
        case None => Ast.PartialApplication(Ast.Variable(name), args.map(processPartialArg))
      }
      case Ast.Invocation(name, args) => Ast.Application(Ast.Value(checkError(name, invocation(name))), args.map(processPartialArg))
      case Ast.PartialInvocation(name, args) => Ast.PartialApplication(Ast.Value(checkError(name, invocation(name))), args.map(processPartialArg))
      case Ast.ShorthandInvocation(name, arg) => Ast.Application(Ast.Value(calc.resolution.globalFunction(name)), Vector(process(arg)))
      case Ast.Application(value, args) => Ast.Application(process(value), args.map(processPartialArg))
      case Ast.PartialApplication(value, args) => Ast.PartialApplication(process(value), args.map(processPartialArg))
      case Ast.SignApplication(name, arg) => Ast.Application(Ast.Value(checkError(name, calc.resolution.sign(name))), Vector(process(arg)))
      case Ast.PostApplication(name, arg) => Ast.Application(Ast.Value(checkError(name, calc.resolution.post(name))), Vector(process(arg)))
      case ops: Ast.OperatorApplication => processOps(ops)
      case _ => elem
    }
    
    def processOps(elem: Ast.OperatorApplication): Ast.Expression = {
      // Make two lists, one with the expressions, one with the operators.
      // Then for each priority we have, slice matching elements out of the list and merge
      // Also process all sub-expressions first
      val expressions: ListBuffer[Ast.Expression] = (elem.head :: elem.tail.map(_._2).toList).map(process).to(ListBuffer)
      val operators: ListBuffer[String] = elem.tail.map(_._1).to(ListBuffer)
      val priorityMap: Map[String, Int] = operators.toSet.map((name: String) => name -> calc.resolution.priority(name)).toMap
      val prioritiesSortedHighToLow: Seq[Int] = priorityMap.values.toSeq.distinct.sortBy(-_)
      for (currentPriority <- prioritiesSortedHighToLow) {
        if (currentPriority % 2 != 0) {
          // right associative
          var i = operators.length - 1
          while (i >= 0) {
            val op: String = operators(i)
            if (priorityMap(op) == currentPriority) {
              val newExpr = Ast.Application(Ast.Value(checkError(op, calc.resolution.operator(op))), Vector(expressions(i), expressions(i + 1)))
              operators.remove(i)
              expressions.remove(i + 1)
              expressions(i) = newExpr
            }
            i -= 1
          }
        } else {
          // left associative
          var i = 0
          while (i < operators.length) {
            val op: String = operators(i)
            if (priorityMap(op) == currentPriority) {
              val newExpr = Ast.Application(Ast.Value(checkError(op, calc.resolution.operator(op))), Vector(expressions(i), expressions(i + 1)))
              operators.remove(i)
              expressions.remove(i + 1)
              expressions(i) = newExpr
              i -= 1
            }
            i += 1
          }
        }
      }
      if (expressions.length != 1) {
        Ast.Value(MathError("Operator resolution failed. This is a bug."))
      } else {
        expressions.head
      }
    }
    
    new BoundExpression(process(expr), expr, errors.toSet)
  }
  
  private case class BoundDefExpression(code: BoundExpression, definitionCode: Ast.Expression) {
    def fullCode: Ast.DefExpression = Ast.DefExpression(code.bound, definitionCode)
  }
}
