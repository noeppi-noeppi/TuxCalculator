package tuxcalculator.core.format

import tuxcalculator.core.expression.Ast
import tuxcalculator.core.function.Descriptor

import java.io.{DataInput, DataOutput}

object AstIO {

  def read(ctx: FormatContext, in: DataInput): Ast.Expression = in.readUnsignedByte() match {
    case 0 => Ast.Group(ctx.ast.get(in.readInt()))
    case 1 => Ast.Answer
    case 2 => Ast.Variable(ctx.strings.get(in.readInt()))
    case 3 => Ast.Value(ctx.values.get(in.readInt()))
    case 4 => Ast.Reference(Ast.DefTarget.Function(ctx.strings.get(in.readInt())))
    case 5 => Ast.Reference(Ast.DefTarget.Operator(ctx.strings.get(in.readInt())))
    case 6 => Ast.Reference(Ast.DefTarget.SignOrOperator(ctx.strings.get(in.readInt())))
    case 7 => Ast.Reference(Ast.DefTarget.Post(ctx.strings.get(in.readInt())))
    case 8 => Ast.Special(ctx.strings.get(in.readInt()))
    case 9 =>
      val len = in.readInt()
      val values = for (_ <- 0 until len) yield readArgument(ctx, in, ArgumentType.Default)
      Ast.List(values.toVector)
    case 10 =>
      val width = in.readInt()
      val values = for (_ <- 0 until width) yield {
        val height = in.readInt()
        (for (_ <- 0 until height) yield ctx.ast.get(in.readInt())).toVector
      }
      Ast.Matrix(values.toVector)
    case 11 =>
      val sig = ctx.signatures.get(in.readInt())
      val code = ctx.ast.get(in.readInt())
      val defCode = ctx.ast.get(in.readInt())
      Ast.Lambda(sig, code, defCode)
    case 12 =>
      val len = in.readInt()
      val entries = for (_ <- 0 until len) yield {
        val sig = ctx.signatures.get(in.readInt())
        val argGuards = for (_ <- sig.names.indices) yield if (in.readBoolean()) {
          Some(Ast.DefExpression(ctx.ast.get(in.readInt()), ctx.ast.get(in.readInt())))
        } else {
          None
        }
        val mainGuard = if (in.readBoolean()) {
          Some(Ast.DefExpression(ctx.ast.get(in.readInt()), ctx.ast.get(in.readInt())))
        } else {
          None
        }
        val code = ctx.ast.get(in.readInt())
        val defCode = ctx.ast.get(in.readInt())
        Ast.MatchEntry(sig, argGuards.toVector, mainGuard, code, defCode)
      }
      Ast.Match(entries.toVector)
    case 13 =>
      val name = ctx.strings.get(in.readInt())
      val len = in.readInt()
      val args = for (_ <- 0 until len) yield readArgument(ctx, in, ArgumentType.Partial)
      Ast.Invocation(name, args.toVector)
    case 14 =>
      val name = ctx.strings.get(in.readInt())
      val len = in.readInt()
      val args = for (_ <- 0 until len) yield readArgument(ctx, in, ArgumentType.Partial)
      Ast.PartialInvocation(name, args.toVector)
    case 15 =>
      val name = ctx.strings.get(in.readInt())
      val arg = ctx.ast.get(in.readInt())
      Ast.ShorthandInvocation(name, arg)
    case 16 =>
      val value = ctx.ast.get(in.readInt())
      val len = in.readInt()
      val args = for (_ <- 0 until len) yield readArgument(ctx, in, ArgumentType.Partial)
      Ast.Application(value, args.toVector)
    case 17 =>
      val value = ctx.ast.get(in.readInt())
      val len = in.readInt()
      val args = for (_ <- 0 until len) yield readArgument(ctx, in, ArgumentType.Partial)
      Ast.PartialApplication(value, args.toVector)
    case 18 => Ast.SignApplication(ctx.strings.get(in.readInt()), ctx.ast.get(in.readInt()))
    case 19 => Ast.PostApplication(ctx.strings.get(in.readInt()), ctx.ast.get(in.readInt()))
    case 20 =>
      val len = in.readInt()
      val head = ctx.ast.get(in.readInt())
      val tail = for (_ <- 0 until len) yield ctx.strings.get(in.readInt()) -> ctx.ast.get(in.readInt())
      Ast.OperatorApplication(head, tail.toVector)
    case b => throw new InvalidFormatException("Corrupted format: Unknown AST type: " + b)
  }
  
  def write(ctx: FormatContext, ast: Ast.Expression, out: DataOutput): Unit = ast match {
    case Ast.Group(expr) => out.writeByte(0)
      out.writeInt(ctx.ast.add(expr))
    case Ast.Answer => out.writeByte(1)
    case Ast.Variable(name) => out.writeByte(2)
      out.writeInt(ctx.strings.add(name))
    case Ast.Value(value) => out.writeByte(3)
      out.writeInt(ctx.values.add(value))
    case Ast.Reference(Ast.DefTarget.Function(name)) => out.writeByte(4)
      out.writeInt(ctx.strings.add(name))
    case Ast.Reference(Ast.DefTarget.Operator(name)) => out.writeByte(5)
      out.writeInt(ctx.strings.add(name))
    case Ast.Reference(Ast.DefTarget.SignOrOperator(name)) => out.writeByte(6)
      out.writeInt(ctx.strings.add(name))
    case Ast.Reference(Ast.DefTarget.Post(name)) => out.writeByte(7)
      out.writeInt(ctx.strings.add(name))
    case Ast.Special(name) => out.writeByte(8)
      out.writeInt(ctx.strings.add(name))
    case Ast.List(values) => out.writeByte(9)
      out.writeInt(values.length)
      for (elem <- values) writeArgument(ctx, elem, out)
    case Ast.Matrix(values) => out.writeByte(10)
      out.writeInt(values.length)
      for (col <- values) {
        out.writeInt(col.length)
        for (elem <- col) out.writeInt(ctx.ast.add(elem))
      }
    case Ast.Lambda(sig, code, defCode) => out.writeByte(11)
      out.writeInt(ctx.signatures.add(sig))
      out.writeInt(ctx.ast.add(code))
      out.writeInt(ctx.ast.add(defCode))
    case Ast.Match(entries) => out.writeByte(12)
      out.writeInt(entries.length)
      for (Ast.MatchEntry(sig, elemGuards, mainGuard, code, defCode) <- entries) {
        out.writeInt(ctx.signatures.add(sig))
        for (guard <- elemGuards) guard match {
          case Some(func) => out.writeBoolean(true)
            out.writeInt(ctx.ast.add(func.code))
            out.writeInt(ctx.ast.add(func.definitionCode))
          case None => out.writeBoolean(false)
        }
        mainGuard match {
          case Some(func) => out.writeBoolean(true)
            out.writeInt(ctx.ast.add(func.code))
            out.writeInt(ctx.ast.add(func.definitionCode))
          case None => out.writeBoolean(false)
        }
        out.writeInt(ctx.ast.add(code))
        out.writeInt(ctx.ast.add(defCode))
      }
    case Ast.Invocation(name, args) => out.writeByte(13)
      out.writeInt(ctx.strings.add(name))
      out.writeInt(args.length)
      for (elem <- args) writeArgument(ctx, elem, out)
    case Ast.PartialInvocation(name, args) => out.writeByte(14)
      out.writeInt(ctx.strings.add(name))
      out.writeInt(args.length)
      for (elem <- args) writeArgument(ctx, elem, out)
    case Ast.ShorthandInvocation(name, arg) => out.writeByte(15)
      out.writeInt(ctx.strings.add(name))
      out.writeInt(ctx.ast.add(arg))
    case Ast.Application(expr, args) => out.writeByte(16)
      out.writeInt(ctx.ast.add(expr))
      out.writeInt(args.length)
      for (elem <- args) writeArgument(ctx, elem, out)
    case Ast.PartialApplication(expr, args) => out.writeByte(17)
      out.writeInt(ctx.ast.add(expr))
      out.writeInt(args.length)
      for (elem <- args) writeArgument(ctx, elem, out)
    case Ast.SignApplication(name, expr) => out.writeByte(18)
      out.writeInt(ctx.strings.add(name))
      out.writeInt(ctx.ast.add(expr))
    case Ast.PostApplication(name, expr) => out.writeByte(19)
      out.writeInt(ctx.strings.add(name))
      out.writeInt(ctx.ast.add(expr))
    case Ast.OperatorApplication(head, tail) => out.writeByte(20)
      out.writeInt(tail.size)
      out.writeInt(ctx.ast.add(head))
      for ((op, value) <- tail) {
        out.writeInt(ctx.strings.add(op))
        out.writeInt(ctx.ast.add(value))
      }
    case _ => throw new IllegalStateException("Can't dump ast: " + ast + " (this is a bug!)")
  }

  private sealed trait ArgumentType[T <: Ast.PartialArgument]
  private object ArgumentType {
    object Partial extends ArgumentType[Ast.PartialArgument]
    object Default extends ArgumentType[Ast.Argument]
  }
  
  private def readArgument[T <: Ast.PartialArgument](ctx: FormatContext, in: DataInput, argType: ArgumentType[T]): T = in.readByte() match {
    case 2 if argType == ArgumentType.Partial => Ast.Placeholder.asInstanceOf[T]
    case 2 => throw new InvalidFormatException("Corrupted format: Placeholder in non-partial argument.")
    case 1 => Ast.SplattedArgument(ctx.ast.get(in.readInt())).asInstanceOf[T]
    case 0 => ctx.ast.get(in.readInt()).asInstanceOf[T]
    case n => throw new InvalidFormatException("Corrupted format: Invalid argument type: " + n)
  }
  
  private def writeArgument(ctx: FormatContext, ast: Ast.PartialArgument, out: DataOutput): Unit = ast match {
    case Ast.Placeholder => out.writeByte(2)
    case Ast.SplattedArgument(expr) => out.writeByte(1)
      out.writeInt(ctx.ast.add(expr))
    case expr: Ast.Expression => out.writeByte(0)
      out.writeInt(ctx.ast.add(expr))
  }

  def readDescriptor(in: DataInput): Descriptor = Descriptor(in.readInt(), in.readBoolean())

  def writeDescriptor(desc: Descriptor, out: DataOutput): Unit = {
    out.writeInt(desc.args)
    out.writeBoolean(desc.vararg)
  }

  def readSignature(ctx: FormatContext, in: DataInput): Ast.Signature = {
    val len = in.readInt()
    val names = for (_ <- 0 until len) yield ctx.strings.get(in.readInt())
    val vararg = in.readBoolean()
    Ast.Signature(names.toVector, vararg)
  }

  def writeSignature(ctx: FormatContext, ast: Ast.Signature, out: DataOutput): Unit = {
    out.writeInt(ast.names.length)
    for (name <- ast.names) out.writeInt(ctx.strings.add(name))
    out.writeBoolean(ast.vararg)
  }
}
