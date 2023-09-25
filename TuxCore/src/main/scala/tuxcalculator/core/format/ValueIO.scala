package tuxcalculator.core.format

import ch.obermuhlner.math.big.BigComplex
import tuxcalculator.core.data.SpecialFunction
import tuxcalculator.core.function.{GlobalFunction, LambdaFunction, MatchFunction, MatchFunctionEntry, MemoizedFunction, MergedOperatorFunction, OperatorFunction, PartialAppliedFunction}
import tuxcalculator.core.value.{MathError, MathFalse, MathFunction, MathList, MathMatrix, MathNumber, MathTrue, MathValue, MathVoid}

import java.io.{DataInput, DataOutput}
import java.math.{BigInteger, BigDecimal => BigDec}

object ValueIO {

  def read(ctx: FormatContext, in: DataInput): MathValue = in.readByte() match {
    case 0 => MathVoid
    case 1 =>
      val msg = ctx.strings.get(in.readInt())
      val len = in.readInt()
      val trace = for (_ <- 0 until len) yield ctx.strings.get(in.readInt())
      MathError(msg, trace.toVector)
    case 2 => MathNumber(BigComplex.valueOf(readNumber(ctx, in)))
    case 3 => MathNumber(BigComplex.valueOf(readNumber(ctx, in), readNumber(ctx, in)))
    case 4 => MathTrue
    case 5 => MathFalse
    case 6 =>
      val len = in.readInt()
      val values = for (_ <- 0 until len) yield ctx.values.get(in.readInt())
      MathList(values.toVector)
    case 7 =>
      val height = in.readInt()
      val width = in.readInt()
      val values = for (_ <- 0 until height) yield (for (_ <- 0 until width) yield ctx.values.get(in.readInt())).toVector
      MathMatrix(values.toVector)
    case 8 => ctx.functions.get(in.readInt());
    case b => throw new InvalidFormatException("Corrupted format: Unknown value type: " + b)
  }

  def write(ctx: FormatContext, value: MathValue, out: DataOutput): Unit = value match {
    case MathVoid => out.writeByte(0)
    case MathError(msg, trace) => out.writeByte(1)
      out.writeInt(ctx.strings.add(msg))
      out.writeInt(trace.length)
      for (line <- trace) out.writeInt(ctx.strings.add(msg))
    case MathNumber(num) if num.im == BigDec.ZERO => out.writeByte(2)
     writeNumber(num.re, out)
    case MathNumber(num) => out.writeByte(3)
     writeNumber(num.re, out)
     writeNumber(num.im, out)
    case MathTrue => out.writeByte(4)
    case MathFalse => out.writeByte(5)
    case MathList(values) => out.writeByte(6)
      out.writeInt(values.length)
      for (elem <- values) out.writeInt(ctx.values.add(elem))
    case mat @ MathMatrix(values) => out.writeByte(7)
      out.writeInt(mat.height)
      out.writeInt(mat.width)
      for (col <- values; elem <- col) out.writeInt(ctx.values.add(elem))
    case func: MathFunction => out.writeByte(8)
      out.writeInt(ctx.functions.add(func))
    case _ => throw new IllegalStateException("Can't dump value: " + value + " (this is a bug!)")
  }

  private def readNumber(ctx: FormatContext, in: DataInput): BigDec = {
    val scale = in.readInt()
    val len = in.readInt()
    if (len < 0) throw new InvalidFormatException("Corrupted format: Negative array length while reading number.")
    val data = new Array[Byte](len)
    in.readFully(data)
    new BigDec(new BigInteger(data), scale)
  }

  private def writeNumber(value: BigDec, out: DataOutput): Unit = {
    out.writeInt(value.scale)
    val data = value.unscaledValue.toByteArray
    out.writeInt(data.length)
    out.write(data)
  }

  def readFunction(ctx: FormatContext, in: DataInput): MathFunction = in.readByte() match {
    case 0 =>
      val name = ctx.strings.get(in.readInt())
      ctx.specials(name) match {
        case func: MathFunction => func
        case value => throw new InvalidFormatException("Corrupted format: Special does not yield a function: " + value)
      }
    case 1 =>
      val sig = ctx.signatures.get(in.readInt())
      val code = ctx.ast.get(in.readInt())
      val defCode = ctx.ast.get(in.readInt())
      new LambdaFunction(sig, code, defCode)
    case 2 =>
      val name = ctx.strings.get(in.readInt())
      val len = in.readInt()
      val map = for (_ <- 0 until len) yield AstIO.readDescriptor(in) -> ctx.functions.get(in.readInt())
      new GlobalFunction(name, map.toMap)
    case 3 => new OperatorFunction(ctx.strings.get(in.readInt()), ctx.functions.get(in.readInt()))
    case 4 => new MergedOperatorFunction(ctx.strings.get(in.readInt()), ctx.functions.get(in.readInt()), ctx.functions.get(in.readInt()))
    case 5 =>
      val value = ctx.values.get(in.readInt())
      val len = in.readInt()
      val args = for (_ <- 0 until len) yield if (in.readBoolean()) Some(ctx.values.get(in.readInt())) else None
      PartialAppliedFunction.create(value, args.toVector)
    case 6 =>
      val len = in.readInt()
      val entries = for (_ <- 0 until len) yield {
        val sig = ctx.signatures.get(in.readInt())
        val argGuards = for (_ <- sig.names.indices) yield if (in.readBoolean()) {
          Some(ctx.functions.get(in.readInt()))
        } else {
          None
        }
        val mainGuard = if (in.readBoolean()) {
          Some(ctx.functions.get(in.readInt()))
        } else {
          None
        }
        val code = ctx.functions.get(in.readInt())
        new MatchFunctionEntry(sig, argGuards.toVector, mainGuard, code)
      }
      val defCode = ctx.ast.get(in.readInt())
      new MatchFunction(entries.toVector, defCode)
    case 7 => new MemoizedFunction(ctx.functions.get(in.readInt()))
    case b => throw new InvalidFormatException("Corrupted format: Unknown function type: " + b)
  }
  
  def writeFunction(ctx: FormatContext, func: MathFunction, out: DataOutput): Unit = func match {
    case special: SpecialFunction => out.writeByte(0)
      out.writeInt(ctx.strings.add(special.name))
    case lambda: LambdaFunction => out.writeByte(1)
      out.writeInt(ctx.signatures.add(lambda.sig))
      out.writeInt(ctx.ast.add(lambda.code))
      out.writeInt(ctx.ast.add(lambda.definitionCode))
    case global: GlobalFunction => out.writeByte(2)
      out.writeInt(ctx.strings.add(global.name))
      out.writeInt(global.map.size)
      for ((k, v) <- global.map) {
        AstIO.writeDescriptor(k, out)
        out.writeInt(ctx.functions.add(v))
      }
    case op: OperatorFunction => out.writeByte(3)
      out.writeInt(ctx.strings.add(op.name))
      out.writeInt(ctx.functions.add(op.function))
    case op: MergedOperatorFunction => out.writeByte(4)
      out.writeInt(ctx.strings.add(op.name))
      out.writeInt(ctx.functions.add(op.function1))
      out.writeInt(ctx.functions.add(op.function2))
    case partial: PartialAppliedFunction => out.writeByte(5)
      out.writeInt(ctx.values.add(partial.value))
      out.writeInt(partial.partialArgs.length)
      for (arg <- partial.partialArgs) arg match {
        case Some(a) => out.writeBoolean(true)
          out.writeInt(ctx.values.add(a))
        case None => out.writeBoolean(false)
      }
    case matched: MatchFunction => out.writeByte(6)
      out.writeInt(matched.entries.length)
      for (entry <- matched.entries) {
        out.writeInt(ctx.signatures.add(entry.sig))
        for (guard <- entry.elementGuards) guard match {
          case Some(func) => out.writeBoolean(true)
            out.writeInt(ctx.functions.add(func))
          case None => out.writeBoolean(false)
        }
        entry.mainGuard match {
          case Some(func) => out.writeBoolean(true)
            out.writeInt(ctx.functions.add(func))
          case None => out.writeBoolean(false)
        }
        out.writeInt(ctx.functions.add(entry.code))
      }
      out.writeInt(ctx.ast.add(matched.definitionCode))
    case memoized: MemoizedFunction => out.writeByte(7)
      out.writeInt(ctx.functions.add(memoized.function))
    case _ => throw new IllegalStateException("Can't dump function: " + func + " (this is a bug!)")
  }
}
