package tuxcalculator.core.format

import tuxcalculator.api.{TuxCalculatorAPI, TuxFrontend}
import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorProperties
import tuxcalculator.core.lexer.{CatCode, FmtCode}

import java.io.{DataInput, DataOutput, EOFException}
import java.text.Normalizer

object FormatIO {
  
  @throws[InvalidFormatException]
  def load(frontend: TuxFrontend, in: DataInput): Calculator = {
    val magic: Int = try {
      val byte1: Int = try in.readByte() & 0xFF catch { case _: EOFException => throw new InvalidFormatException("Empty format file.") }
      val byte2: Int = in.readByte() & 0xFF
      val byte3: Int = in.readByte() & 0xFF
      val byte4: Int = in.readByte() & 0xFF
      (byte1 << 24) | (byte2 << 16) | (byte3 << 8) | byte4
    } catch {
      case _: EOFException => throw new InvalidFormatException("Not a TuxCalculator Format file.");
    }
    if (magic != 0x40956A19) throw new InvalidFormatException("Not a TuxCalculator Format file.")
    val ver = in.readUTF()
    if (ver != TuxCalculatorAPI.VERSION) throw new InvalidFormatException("Format file was compiled for version " + ver +  ". Can't load it on version " + TuxCalculatorAPI.VERSION + ".")
    
    val calc = new Calculator(frontend, ini = false)
    
    val catLen = in.readInt()
    for (_ <- 0 until catLen) {
      calc.lexer.catCode(in.readInt(), CatCode(in.readByte()))
    }
    
    val tokLen = in.readInt()
    for (_ <- 0 until tokLen) {
      calc.lexer.tokCode(in.readUTF(), CatCode(in.readByte()))
    }
    
    val fmtLen = in.readInt()
    for (_ <- 0 until fmtLen) {
      calc.lexer.fmtCode(FmtCode(in.readByte()), in.readUTF())
    }
    
    calc.properties.set(CalculatorProperties.Precision, in.readInt())
    calc.properties.set(CalculatorProperties.Output, in.readInt())
    calc.properties.set(CalculatorProperties.Truncate, in.readInt())
    calc.properties.set(CalculatorProperties.Eager, in.readBoolean())
    calc.properties.set(CalculatorProperties.Normalization, in.readUnsignedByte() match {
      case 0xFF => None
      case ordinal => Normalizer.Form.values() match {
        case values if values.indices contains ordinal => Some(values(ordinal))
        case _ => throw new InvalidFormatException("Invalid input normalization in format.")
      }
    })
    calc.properties.set(CalculatorProperties.Highlight, in.readBoolean())
    calc.properties.set(CalculatorProperties.Polar, in.readByte() match {
      case 0 => CalculatorProperties.PolarType.None
      case 1 => CalculatorProperties.PolarType.Radians
      case 2 => CalculatorProperties.PolarType.Degrees
      case _ => throw new InvalidFormatException("Invalid polar formatting in format.")
    })
    calc.properties.set(CalculatorProperties.Autoref, in.readBoolean())
    
    val answer = calc.resolution.read(in)
    calc.finish(answer)
    
    calc
  }
  
  def dump(calc: Calculator, out: DataOutput): Unit = {
    out.writeInt(0x40956A19)
    out.writeUTF(TuxCalculatorAPI.VERSION)
    
    val catCodes = calc.lexer.allChangedCatCodes
    out.writeInt(catCodes.size)
    for ((codePoint, catCode) <- catCodes.toSeq.sortBy(_._1)) {
      if (catCode.id >= 128) throw new IllegalStateException("Can't write catcode " + catCode + ". This is a bug.")
      out.writeInt(codePoint)
      out.writeByte(catCode.id)
    }
    
    val tokCodes = calc.lexer.allChangedTokCodes
    out.writeInt(tokCodes.size)
    for ((token, catCode) <- tokCodes.toSeq.sortBy(_._1)) {
      if (catCode.id >= 128) throw new IllegalStateException("Can't write catcode " + catCode + ". This is a bug.")
      out.writeUTF(token)
      out.writeByte(catCode.id)
    }
    
    val fmtCodes = calc.lexer.allChangedFmtCodes
    out.writeInt(fmtCodes.size)
    for ((fmtCode, format) <- fmtCodes.toSeq.sortBy(_._1)) {
      if (fmtCode.id >= 128) throw new IllegalStateException("Can't write fmtcode " + fmtCode + ". This is a bug.")
      out.writeByte(fmtCode.id)
      out.writeUTF(format)
    }
    
    out.writeInt(calc.properties(CalculatorProperties.Precision))
    out.writeInt(calc.properties(CalculatorProperties.Output))
    out.writeInt(calc.properties(CalculatorProperties.Truncate))
    out.writeBoolean(calc.properties(CalculatorProperties.Eager))
    calc.properties(CalculatorProperties.Normalization) match {
      case Some(normalization) if normalization.ordinal() >= 128 => throw new IllegalStateException("Can't write normalization " + normalization + ". This is a bug.")
      case Some(normalization) => out.writeByte(normalization.ordinal())
      case None => out.writeByte(0xFF)
    }
    out.writeBoolean(calc.properties(CalculatorProperties.Highlight))
    calc.properties(CalculatorProperties.Polar) match {
      case CalculatorProperties.PolarType.None => out.writeByte(0)
      case CalculatorProperties.PolarType.Radians => out.writeByte(1)
      case CalculatorProperties.PolarType.Degrees => out.writeByte(2)
    }
    out.writeBoolean(calc.properties(CalculatorProperties.Autoref))
    
    calc.resolution.write(out)
  }
}
