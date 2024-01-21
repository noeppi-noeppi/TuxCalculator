package tuxcalculator.core.resolution

import tuxcalculator.core.Calculator
import tuxcalculator.core.expression.{Ast, ExpressionHelper}
import tuxcalculator.core.format.{AstIO, FormatContext}
import tuxcalculator.core.function.{BracketFunction, GlobalFunction, ChainedOperatorFunction, OperatorFunction}
import tuxcalculator.core.value.{MathError, MathNumber, MathValue, MathVoid}

import java.io.{ByteArrayOutputStream, DataInput, DataOutput, DataOutputStream}
import scala.collection.mutable

class ResolutionTable(private val calc: Calculator) {
  
  private[this] val functions: mutable.Map[String, GlobalFunction] = mutable.Map()
  private[this] val variables: mutable.Map[String, MathValue] = mutable.Map()
  private[this] val operators: mutable.Map[String, OperatorFunction] = mutable.Map()
  private[this] val signs: mutable.Map[String, OperatorFunction] = mutable.Map()
  private[this] val postfixes: mutable.Map[String, OperatorFunction] = mutable.Map()
  private[this] val primaries: mutable.Map[String, BracketFunction] = mutable.Map()
  private[this] val secondaries: mutable.Map[String, BracketFunction] = mutable.Map()
  private[this] val tertiaries: mutable.Map[String, BracketFunction] = mutable.Map()
  
  private[this] val priorities: mutable.Map[String, Int] = mutable.Map()
  
  def priority(name: String): Int = priorities.getOrElse(name, 0)
  def variable(name: String): MathValue = variables.getOrElse(name, MathError("Unbound value: '" + name + "'"))
  def globalFunction(name: String): MathValue = functions.getOrElse(name, MathError("Unbound global function: '" + name + "'"))
  def maybeGlobalFunction(name: String): Option[MathValue] = functions.get(name)
  def operator(name: String): MathValue = operators.getOrElse(name, MathError("Unbound operator: '" + name + "'"))
  def sign(name: String): MathValue = signs.getOrElse(name, MathError("Unbound sign operator: '" + name + "'"))
  def post(name: String): MathValue = postfixes.getOrElse(name, MathError("Unbound postfix: '" + name + "'"))
  def primaryBracket(name: String, close: String): MathValue = primaries.get(name).map(f => if (close == f.close) f else MathError("Invalid closing bracket for " + name + ", expected " + f.close + ", got " + close)).getOrElse(MathError("Unbound primary bracket: '" + name + "'"))
  def secondaryBracket(name: String, close: String): MathValue = secondaries.get(name).map(f => if (close == f.close) f else MathError("Invalid closing bracket for " + name + ", expected " + f.close + ", got " + close)).getOrElse(MathError("Unbound secondary bracket: '" + name + "'"))
  def tertiaryBracket(name: String, close: String): MathValue = tertiaries.get(name).map(f => if (close == f.close) f else MathError("Invalid closing bracket for " + name + ", expected " + f.close + ", got " + close)).getOrElse(MathError("Unbound tertiary bracket: '" + name + "'"))
  def reference(target: Ast.DefTarget): MathValue = target match {
    case Ast.DefTarget.Function(name) => functions.getOrElse(name, MathError("Unbound global function: '" + name + "'"))
    case Ast.DefTarget.Operator(name) if operators.contains(name) => new ChainedOperatorFunction(name, priority(name), None, Some(operators(name)))
    case Ast.DefTarget.SignOrOperator(name) if operators.contains(name) || signs.contains(name) => new ChainedOperatorFunction(name, priority(name), signs.get(name), operators.get(name))
    case Ast.DefTarget.Post(name) if postfixes.contains(name) => new ChainedOperatorFunction(name, 0, postfixes.get(name), None)
    case Ast.DefTarget.PrimaryBracket(name, close) => primaryBracket(name, close)
    case Ast.DefTarget.SecondaryBracket(name, close) => secondaryBracket(name, close)
    case Ast.DefTarget.TertiaryBracket(name, close) => tertiaryBracket(name, close)
    // These will always yield MathError
    case Ast.DefTarget.Operator(name) => operator(name)
    case Ast.DefTarget.SignOrOperator(name) => sign(name)
    case Ast.DefTarget.Post(name) => post(name)
  }

  def tabCompleteIdentifier: Set[String] = (functions.keySet | variables.keySet).toSet
  def tabCompleteReference: Set[String] = (functions.keySet | signs.keySet | operators.keySet | postfixes.keySet | primaries.map(e => e._1 + e._2.close).toSet| secondaries.map(e => e._1 + e._2.close).toSet | tertiaries.map(e => e._1 + e._2.close).toSet).toSet

  def let(name: String, value: MathValue): MathValue = {
    variables(name) = value
    value
  }
  
  def define(target: Ast.DefTarget, sig: Ast.Signature, expr: Ast.Expression): MathValue = target match {
    case Ast.DefTarget.Function(name) =>
      functions(name) = functions.getOrElse(name, new GlobalFunction(name)).extend(sig, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.SecondaryBracket(name, close) if sig.names.size == 1 && sig.vararg =>
      secondaries(name) = new BracketFunction(name, close, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.TertiaryBracket(name, close) if (sig.names.size == 1 || sig.names.size == 2 || sig.names.size == 3) && sig.vararg =>
      tertiaries(name) = new BracketFunction(name, close, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.SecondaryBracket(_, _) => MathError("Defining a secondary bracket needs a vararg function without fixed arguments.")
    case Ast.DefTarget.TertiaryBracket(_, _) => MathError("Defining a tertiary bracket needs a vararg function with at most two fixed arguments.")
    case _ if sig.vararg => MathError("Can't define vararg operators")
    case Ast.DefTarget.Operator(name) if sig.names.length == 2 =>
      operators(name) = new OperatorFunction(name, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.SignOrOperator(name) if sig.names.length == 2 =>
      operators(name) = new OperatorFunction(name, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.SignOrOperator(name) if sig.names.length == 1 =>
      signs(name) = new OperatorFunction(name, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.Post(name) if sig.names.length == 1 =>
      postfixes(name) = new OperatorFunction(name, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.PrimaryBracket(name, close) if sig.names.length == 1 =>
      primaries(name) = new BracketFunction(name, close, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.Operator(_) => MathError("Defining an operator needs a function with two arguments.")
    case Ast.DefTarget.SignOrOperator(_) => MathError("Defining a sign operator needs a function with one or two arguments.")
    case Ast.DefTarget.Post(_) => MathError("Defining a postfix needs a function with one argument.")
    case Ast.DefTarget.PrimaryBracket(_, _) => MathError("Defining a primary bracket needs a function with one argument.")
  }
  
  def remove(target: Ast.DefTarget): MathValue = target match {
    case Ast.DefTarget.Function(name) => MathNumber(BigDecimal(functions.remove(name).map(_.definitionCount).size + variables.remove(name).size))
    case Ast.DefTarget.Operator(name) => MathNumber(BigDecimal(operators.remove(name).size))
    case Ast.DefTarget.SignOrOperator(name) => MathNumber(BigDecimal(operators.remove(name).size + signs.remove(name).size))
    case Ast.DefTarget.Post(name) => MathNumber(BigDecimal(postfixes.remove(name).size))
    case Ast.DefTarget.PrimaryBracket(name, close) =>
      val oldSize: Int = primaries.size
      primaries.filterInPlace((key, value) => name != key || close != value.close)
      MathNumber(BigDecimal(oldSize - primaries.size))
    case Ast.DefTarget.SecondaryBracket(name, close) =>
      val oldSize: Int = secondaries.size
      secondaries.filterInPlace((key, value) => name != key || close != value.close)
      MathNumber(BigDecimal(oldSize - secondaries.size))
    case Ast.DefTarget.TertiaryBracket(name, close) =>
      val oldSize: Int = tertiaries.size
      tertiaries.filterInPlace((key, value) => name != key || close != value.close)
      MathNumber(BigDecimal(oldSize - tertiaries.size))
  }
  
  def priority(name: String, priority: Int): Unit = priorities(name) = priority

  def read(in: DataInput): MathValue = {
    val ctx = new FormatContext(calc.specials, in)

    functions.clear()
    val funcLen = in.readInt()
    for (_ <- 0 until funcLen) {
      val name = ctx.strings.get(in.readInt())
      val implLen = in.readInt()
      val map = for (_ <- 0 until implLen) yield AstIO.readDescriptor(in) -> ctx.functions.get(in.readInt())
      functions(name) = new GlobalFunction(name, map.toMap)
    }

    variables.clear()
    val varLen = in.readInt()
    for (_ <- 0 until varLen) {
      variables(ctx.strings.get(in.readInt())) = ctx.values.get(in.readInt())
    }

    operators.clear()
    val opLen = in.readInt()
    for (_ <- 0 until opLen) {
      val name = ctx.strings.get(in.readInt())
      operators(name) = new OperatorFunction(name, ctx.functions.get(in.readInt()))
    }

    signs.clear()
    val signLen = in.readInt()
    for (_ <- 0 until signLen) {
      val name = ctx.strings.get(in.readInt())
      signs(name) = new OperatorFunction(name, ctx.functions.get(in.readInt()))
    }

    postfixes.clear()
    val postLen = in.readInt()
    for (_ <- 0 until postLen) {
      val name = ctx.strings.get(in.readInt())
      postfixes(name) = new OperatorFunction(name, ctx.functions.get(in.readInt()))
    }
    
    primaries.clear()
    val primaryLen = in.readInt()
    for (_ <- 0 until primaryLen) {
      val name = ctx.strings.get(in.readInt())
      val close = ctx.strings.get(in.readInt())
      primaries(name) = new BracketFunction(name, close, ctx.functions.get(in.readInt()))
    }
    
    secondaries.clear()
    val secondaryLen = in.readInt()
    for (_ <- 0 until secondaryLen) {
      val name = ctx.strings.get(in.readInt())
      val close = ctx.strings.get(in.readInt())
      secondaries(name) = new BracketFunction(name, close, ctx.functions.get(in.readInt()))
    }
    
    tertiaries.clear()
    val tertiaryLen = in.readInt()
    for (_ <- 0 until tertiaryLen) {
      val name = ctx.strings.get(in.readInt())
      val close = ctx.strings.get(in.readInt())
      tertiaries(name) = new BracketFunction(name, close, ctx.functions.get(in.readInt()))
    }

    priorities.clear()
    val prioLen = in.readInt()
    for (_ <- 0 until prioLen) {
      priorities(ctx.strings.get(in.readInt())) = in.readInt()
    }

    ctx.values.get(in.readInt())
  }
  
  def write(out: DataOutput): Unit = {
    val ctx = new FormatContext(calc.specials)
    
    val tableData = new ByteArrayOutputStream()
    val dataOut = new DataOutputStream(tableData)

    dataOut.writeInt(functions.size)
    for ((name, func) <- functions.toVector.sortBy(_._1)) {
      dataOut.writeInt(ctx.strings.add(name))
      dataOut.writeInt(func.map.size)
      for ((desc, impl) <- func.map) {
        AstIO.writeDescriptor(desc, dataOut)
        dataOut.writeInt(ctx.functions.add(impl))
      }
    }

    dataOut.writeInt(variables.size)
    for ((name, value) <- variables.toVector.sortBy(_._1)) {
      dataOut.writeInt(ctx.strings.add(name))
      dataOut.writeInt(ctx.values.add(value))
    }

    dataOut.writeInt(operators.size)
    for ((name, impl) <- operators.toVector.sortBy(_._1)) {
      dataOut.writeInt(ctx.strings.add(name))
      dataOut.writeInt(ctx.functions.add(impl.function))
    }

    dataOut.writeInt(signs.size)
    for ((name, impl) <- signs.toVector.sortBy(_._1)) {
      dataOut.writeInt(ctx.strings.add(name))
      dataOut.writeInt(ctx.functions.add(impl.function))
    }

    dataOut.writeInt(postfixes.size)
    for ((name, impl) <- postfixes.toVector.sortBy(_._1)) {
      dataOut.writeInt(ctx.strings.add(name))
      dataOut.writeInt(ctx.functions.add(impl.function))
    }
    
    dataOut.writeInt(primaries.size)
    for ((name, impl) <- primaries.toVector.sortBy(_._1)) {
      dataOut.writeInt(ctx.strings.add(name))
      dataOut.writeInt(ctx.strings.add(impl.close))
      dataOut.writeInt(ctx.functions.add(impl.function))
    }

    dataOut.writeInt(secondaries.size)
    for ((name, impl) <- secondaries.toVector.sortBy(_._1)) {
      dataOut.writeInt(ctx.strings.add(name))
      dataOut.writeInt(ctx.strings.add(impl.close))
      dataOut.writeInt(ctx.functions.add(impl.function))
    }

    dataOut.writeInt(tertiaries.size)
    for ((name, impl) <- tertiaries.toVector.sortBy(_._1)) {
      dataOut.writeInt(ctx.strings.add(name))
      dataOut.writeInt(ctx.strings.add(impl.close))
      dataOut.writeInt(ctx.functions.add(impl.function))
    }
    
    val changedPriorities: Map[String, Int] = priorities.toMap.filter(_._2 != 0)
    dataOut.writeInt(changedPriorities.size)
    for ((name, priority) <- changedPriorities.toVector.sortBy(_._1)) {
      dataOut.writeInt(ctx.strings.add(name))
      dataOut.writeInt(priority)
    }
    
    dataOut.writeInt(ctx.values.add(calc.answer))
    
    dataOut.close()
    ctx.writeSymbols(out)
    out.write(tableData.toByteArray)
  }
}
