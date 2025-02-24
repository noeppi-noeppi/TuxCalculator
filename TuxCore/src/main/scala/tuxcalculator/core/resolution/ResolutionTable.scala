package tuxcalculator.core.resolution

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorProperties
import tuxcalculator.core.expression.{Ast, ExpressionHelper}
import tuxcalculator.core.format.{AstIO, FormatContext}
import tuxcalculator.core.function.{BracketFunction, ChainedOperatorFunction, GlobalFunction, OperatorFunction}
import tuxcalculator.core.value.{MathError, MathNumber, MathValue, MathVoid}

import java.io.{ByteArrayOutputStream, DataInput, DataOutput, DataOutputStream}
import scala.collection.mutable

class ResolutionTable(private val calc: Calculator) {
  
  case class Bracket(open: String, close: String) extends Ordered[Bracket] {
    def name: String = open + close
    override def compare(that: Bracket): Int = Ordering.String.compare(open, that.open) match {
      case 0 => Ordering.String.compare(close, that.close)
      case n => n
    }
  }
  
  private[this] val functions: mutable.Map[String, GlobalFunction] = mutable.Map()
  private[this] val variables: mutable.Map[String, MathValue] = mutable.Map()
  private[this] val operators: mutable.Map[String, OperatorFunction] = mutable.Map()
  private[this] val signs: mutable.Map[String, OperatorFunction] = mutable.Map()
  private[this] val postfixes: mutable.Map[String, OperatorFunction] = mutable.Map()
  private[this] val primaries: mutable.Map[Bracket, BracketFunction] = mutable.Map()
  private[this] val secondaries: mutable.Map[Bracket, BracketFunction] = mutable.Map()
  private[this] val tertiaries: mutable.Map[Bracket, BracketFunction] = mutable.Map()
  
  private[this] val priorities: mutable.Map[String, Int] = mutable.Map()
  
  private[this] var frontendErrorOnUnboundValue: Boolean = false
  
  def priority(name: String): Int = priorities.getOrElse(name, 0)
  def variable(name: String): MathValue = variables.get(name) match {
    case Some(variable) => variable
    case None if calc.properties(CalculatorProperties.Autoref) => maybeGlobalFunction(name: String).getOrElse(unbound("Unbound value: '" + name + "'"))
    case None => unbound("Unbound value: '" + name + "'")
  }
  def globalFunction(name: String): MathValue = functions.getOrElse(name, unbound("Unbound global function: '" + name + "'"))
  def maybeGlobalFunction(name: String): Option[MathValue] = functions.get(name)
  def operator(name: String): MathValue = operators.getOrElse(name, unbound("Unbound operator: '" + name + "'"))
  def sign(name: String): MathValue = signs.getOrElse(name, unbound("Unbound sign operator: '" + name + "'"))
  def post(name: String): MathValue = postfixes.getOrElse(name, unbound("Unbound postfix: '" + name + "'"))
  def primaryBracket(open: String, close: String): MathValue = bracket(open, close, "primary", primaries)
  def secondaryBracket(open: String, close: String): MathValue = bracket(open, close, "secondary", secondaries)
  def tertiaryBracket(open: String, close: String): MathValue = bracket(open, close, "tertiary", tertiaries)
  private def bracket(open: String, close: String, typeName: String, table: mutable.Map[Bracket, BracketFunction]): MathValue = table.get(Bracket(open, close)) match {
    case Some(function) => function
    case None =>
      val alternatives: Seq[String] = table.keys.filter(bracket => open == bracket.open).map(bracket => bracket.close).toSeq.sorted
      alternatives.size match {
        case 0 => unbound("Unbound " + typeName + " bracket: '" + open + close + "'")
        case 1 => unbound("Invalid closing bracket for " + open + ", expected " + alternatives.head + ", got " + close)
        case _ => unbound("Invalid closing bracket for " + open + ", expected one of " + alternatives.mkString(", ") + ", got " + close)
      }
  }
  def reference(target: Ast.DefTarget): MathValue = target match {
    case Ast.DefTarget.Function(name) => functions.getOrElse(name, unbound("Unbound global function: '" + name + "'"))
    case Ast.DefTarget.Operator(name) if operators.contains(name) => new ChainedOperatorFunction(name, priority(name), None, Some(operators(name)))
    case Ast.DefTarget.SignOrOperator(name) if operators.contains(name) || signs.contains(name) => new ChainedOperatorFunction(name, priority(name), signs.get(name), operators.get(name))
    case Ast.DefTarget.Post(name) if postfixes.contains(name) => new ChainedOperatorFunction(name, 0, postfixes.get(name), None)
    case Ast.DefTarget.PrimaryBracket(open, close) => primaryBracket(open, close)
    case Ast.DefTarget.SecondaryBracket(open, close) => secondaryBracket(open, close)
    case Ast.DefTarget.TertiaryBracket(open, close) => tertiaryBracket(open, close)
    // These will always yield MathError
    case Ast.DefTarget.Operator(name) => operator(name)
    case Ast.DefTarget.SignOrOperator(name) => sign(name)
    case Ast.DefTarget.Post(name) => post(name)
  }

  def tabCompleteIdentifier: Set[String] = (functions.keySet | variables.keySet).toSet
  def tabCompleteReferenceFunction: Set[String] = functions.keySet.toSet
  def tabCompleteReferenceOperator: Set[String] = (signs.keySet | operators.keySet | postfixes.keySet | primaries.keySet.map(_.name) | secondaries.keySet.map(_.name) | tertiaries.keySet.map(_.name)).toSet

  def produceFrontendErrorOnUnboundValue(): Unit = frontendErrorOnUnboundValue = true
  private def unbound(message: String): MathValue = {
    if (frontendErrorOnUnboundValue) calc.frontend.showError(message)
    MathError(message)
  }
  
  def let(name: String, value: MathValue): MathValue = {
    variables(name) = value
    value
  }
  
  def define(target: Ast.DefTarget, sig: Ast.Signature, expr: Ast.Expression): MathValue = target match {
    case Ast.DefTarget.Function(name) =>
      functions(name) = functions.getOrElse(name, new GlobalFunction(name)).extend(sig, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.SecondaryBracket(open, close) if sig.names.size == 1 && sig.vararg =>
      secondaries(Bracket(open, close)) = new BracketFunction(open, close, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.TertiaryBracket(open, close) if (sig.names.size == 1 || sig.names.size == 2 || sig.names.size == 3) && sig.vararg =>
      tertiaries(Bracket(open, close)) = new BracketFunction(open, close, ExpressionHelper.makeLambdaLike(calc, sig, expr))
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
    case Ast.DefTarget.PrimaryBracket(open, close) if sig.names.length == 1 =>
      primaries(Bracket(open, close)) = new BracketFunction(open, close, ExpressionHelper.makeLambdaLike(calc, sig, expr))
      MathVoid
    case Ast.DefTarget.Operator(_) => MathError("Defining an operator needs a function with two arguments.")
    case Ast.DefTarget.SignOrOperator(_) => MathError("Defining a sign operator needs a function with one or two arguments.")
    case Ast.DefTarget.Post(_) => MathError("Defining a postfix needs a function with one argument.")
    case Ast.DefTarget.PrimaryBracket(_, _) => MathError("Defining a primary bracket needs a function with one argument.")
  }
  
  def remove(target: Ast.DefTarget): MathValue = target match {
    case Ast.DefTarget.Function(name) => MathNumber(BigDecimal(functions.remove(name).map(_.definitionCount).getOrElse(0) + variables.remove(name).size))
    case Ast.DefTarget.Operator(name) => MathNumber(BigDecimal(operators.remove(name).size))
    case Ast.DefTarget.SignOrOperator(name) => MathNumber(BigDecimal(operators.remove(name).size + signs.remove(name).size))
    case Ast.DefTarget.Post(name) => MathNumber(BigDecimal(postfixes.remove(name).size))
    case Ast.DefTarget.PrimaryBracket(open, close) => MathNumber(primaries.remove(Bracket(open, close)).size)
    case Ast.DefTarget.SecondaryBracket(open, close) => MathNumber(secondaries.remove(Bracket(open, close)).size)
    case Ast.DefTarget.TertiaryBracket(open, close) => MathNumber(tertiaries.remove(Bracket(open, close)).size)
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
    
    def readOperators(table: mutable.Map[String, OperatorFunction]): Unit = {
      table.clear()
      val len = in.readInt()
      for (_ <- 0 until len) {
        val name = ctx.strings.get(in.readInt())
        table(name) = new OperatorFunction(name, ctx.functions.get(in.readInt()))
      }
    }
    
    readOperators(operators)
    readOperators(signs)
    readOperators(postfixes)
    
    def readBrackets(table: mutable.Map[Bracket, BracketFunction]): Unit = {
      table.clear()
      val len = in.readInt()
      for (_ <- 0 until len) {
        val open = ctx.strings.get(in.readInt())
        val close = ctx.strings.get(in.readInt())
        table(Bracket(open, close)) =  new BracketFunction(open, close, ctx.functions.get(in.readInt()))
      }
    }
    
    readBrackets(primaries)
    readBrackets(secondaries)
    readBrackets(tertiaries)

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

    def writeOperators(table: collection.Map[String, OperatorFunction]): Unit = {
      dataOut.writeInt(table.size)
      for ((name, impl) <- table.toVector.sortBy(_._1)) {
        dataOut.writeInt(ctx.strings.add(name))
        dataOut.writeInt(ctx.functions.add(impl.function))
      }
    }
    
    writeOperators(operators)
    writeOperators(signs)
    writeOperators(postfixes)

    def writeBrackets(table: mutable.Map[Bracket, BracketFunction]): Unit = {
      dataOut.writeInt(table.size)
      for ((Bracket(open, close), impl) <- table.toVector.sortBy(_._1)) {
        dataOut.writeInt(ctx.strings.add(open))
        dataOut.writeInt(ctx.strings.add(close))
        dataOut.writeInt(ctx.functions.add(impl.function))
      }
    }
    
    writeBrackets(primaries)
    writeBrackets(secondaries)
    writeBrackets(tertiaries)
    
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
