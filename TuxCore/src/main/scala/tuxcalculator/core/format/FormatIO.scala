package tuxcalculator.core.format

import tuxcalculator.api.{TuxCalculatorAPI, TuxFrontend}
import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorProperties
import tuxcalculator.core.lexer.CatCode

import java.io.{DataInput, DataOutput}
import java.text.Normalizer

object FormatIO {
  
  @throws[InvalidFormatException]
  def load(frontend: TuxFrontend, in: DataInput): Calculator = {
    val magic = in.readInt()
    if (magic != 0x40956A19) throw new InvalidFormatException("Invalid format file")
    val ver = in.readUTF()
    if (ver != TuxCalculatorAPI.VERSION) throw new InvalidFormatException("Format file was made for version " + ver +  ". Can't load it on version " + TuxCalculatorAPI.VERSION + ".")
    
    val calc = new Calculator(frontend, ini = false)
    
    val catLen = in.readInt()
    for (_ <- 0 until catLen) {
      calc.lexer.catCode(in.readInt(), CatCode(in.readByte()))
    }
    
    val tokLen = in.readInt()
    for (_ <- 0 until tokLen) {
      calc.lexer.tokCode(in.readUTF(), CatCode(in.readByte()))
    }
    
    calc.properties.set(CalculatorProperties.Precision, in.readInt())
    calc.properties.set(CalculatorProperties.Output, in.readInt())
    calc.properties.set(CalculatorProperties.Truncate, in.readInt())
    calc.properties.set(CalculatorProperties.Eager, in.readBoolean())
    calc.properties.set(CalculatorProperties.Normalization, in.readByte() match {
      case 0xFF => None
      case ordinal => Normalizer.Form.values() match {
        case values if values.indices contains ordinal => Some(values(ordinal))
        case _ => throw new IllegalStateException("Invalid input normalization in format.")
      }
    })
    calc.properties.set(CalculatorProperties.Highlight, in.readBoolean())
    calc.properties.set(CalculatorProperties.Polar, in.readByte() match {
      case 0 => CalculatorProperties.PolarType.None
      case 1 => CalculatorProperties.PolarType.Radians
      case 2 => CalculatorProperties.PolarType.Degrees
      case _ => throw new IllegalStateException("Invalid polar formatting in format.")
    })
    
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
    
    calc.resolution.write(out)
  }
}
