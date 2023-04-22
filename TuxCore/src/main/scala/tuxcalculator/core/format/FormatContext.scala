package tuxcalculator.core.format

import tuxcalculator.core.data.CalculatorSpecials
import tuxcalculator.core.expression.Ast
import tuxcalculator.core.value.{MathFunction, MathValue}

import java.io.{DataInput, DataOutput}

class FormatContext private(val specials: CalculatorSpecials, in: Option[DataInput]) {
  
  def this(specials: CalculatorSpecials) = this(specials, None)
  def this(specials: CalculatorSpecials, in: DataInput) = this(specials, Some(in))
  
  val strings: SymbolTable[String] = new SymbolTable[String]("strings", new SymbolEncoder[String] {
    override def read(in: DataInput): String = in.readUTF()
    override def write(value: String, out: DataOutput): Unit = out.writeUTF(value)
  }, in)
  
  val values: SymbolTable[MathValue] = new SymbolTable[MathValue]("values", new SymbolEncoder[MathValue] {
    override def read(in: DataInput): MathValue = ValueIO.read(FormatContext.this, in)
    override def write(value: MathValue, out: DataOutput): Unit = ValueIO.write(FormatContext.this, value, out)
  }, in)
  
  val functions: SymbolTable[MathFunction] = new SymbolTable[MathFunction]("functions", new SymbolEncoder[MathFunction] {
    override def read(in: DataInput): MathFunction = ValueIO.readFunction(FormatContext.this, in)
    override def write(value: MathFunction, out: DataOutput): Unit = ValueIO.writeFunction(FormatContext.this, value, out)
  }, in)
  
  val signatures: SymbolTable[Ast.Signature] = new SymbolTable[Ast.Signature]("signatures", new SymbolEncoder[Ast.Signature] {
    override def read(in: DataInput): Ast.Signature = AstIO.readSignature(FormatContext.this, in)
    override def write(value: Ast.Signature, out: DataOutput): Unit = AstIO.writeSignature(FormatContext.this, value, out)
  }, in)
  
  val ast: SymbolTable[Ast.Expression] = new SymbolTable[Ast.Expression]("ast", new SymbolEncoder[Ast.Expression] {
    override def read(in: DataInput): Ast.Expression = AstIO.read(FormatContext.this, in)
    override def write(value: Ast.Expression, out: DataOutput): Unit = AstIO.write(FormatContext.this, value, out)
  }, in)
  
  def writeSymbols(out: DataOutput): Unit = {
    strings.write(out)
    values.write(out)
    functions.write(out)
    signatures.write(out)
    ast.write(out)
  }
}
