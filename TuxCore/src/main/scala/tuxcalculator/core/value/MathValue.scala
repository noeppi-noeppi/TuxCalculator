package tuxcalculator.core.value

import ch.obermuhlner.math.big.BigComplex
import tuxcalculator.core.Calculator

sealed trait MathValue {
  def number(calc: Calculator): NumericValue = MathError("Number expected, got: '" + calc.format(this) + "'")
  def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = MathError("Can't apply '" + calc.format(this) + "'")
}

sealed trait NumericValue extends MathValue {
  override def number(calc: Calculator): NumericValue = this
}

object MathVoid extends NumericValue {
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = MathVoid
}

case class MathError(msg: String, trace: Vector[String]) extends NumericValue {
  def trace(traceMsg: String): MathError = MathError(msg, trace.appended(traceMsg))
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = this.trace("Application: (" + args.map(calc.format).mkString(", ") + ")")
}

case class MathNumber(num: BigComplex) extends NumericValue

case object MathTrue extends MathValue {
  override def number(calc: Calculator): NumericValue = MathNumber(BigDecimal(1, calc.mathContext))
}

case object MathFalse extends MathValue {
  override def number(calc: Calculator): NumericValue = MathNumber(BigDecimal(0, calc.mathContext))
}

case class MathList(values: Vector[MathValue]) extends MathValue {
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = {
    if (args.length != 1 ) MathError("Lists can only be applied to a single argument") else ValueHelper.run(calc) {
      val idx = ValueHelper.realInt(args.head)
      if (idx < 0 || idx >= values.length) ValueHelper.error("Index " + idx + " out of bounds for length " + values.length)
      values(idx.toInt)
    }
  }
}

case class MathMatrix(values: Vector[Vector[MathValue]]) extends MathValue {
  if (values.nonEmpty && values.map(_.length).distinct.size != 1) {
    throw new IllegalArgumentException("Matrix with different column sizes. This is a bug.")
  } else if (values.isEmpty || values.exists(_.isEmpty)) {
    throw new IllegalArgumentException("Wrong empty matrix. This is a bug.")
  }
  
  val width: Int = values.length
  val height: Int = if (values.isEmpty) 0 else values.head.length
  
  def get(row: Int, col: Int): MathValue = values(col)(row)
  def getRow(row: Int): Vector[MathValue] = values.map(_(row))
  def getCol(col: Int): Vector[MathValue] = values(col)
  def indices: Iterable[(Int, Int)] = for (row <- 0 until height; col <- 0 until width) yield (row, col)
  
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = {
    if (args.length == 1 && values.length == 1) {
      ValueHelper.run(calc) {
        val idx = ValueHelper.realInt(args.head)
        if (idx <= 0 || idx > values.head.length) ValueHelper.error("Index " + idx + " out of bounds for vector length " + values.head.length)
        values.head(idx.toInt - 1)
      }
    } else if (args.length == 2) {
      ValueHelper.run(calc) {
        val row = ValueHelper.realInt(args(0))
        val col = ValueHelper.realInt(args(1))
        if (col <= 0 || col > values.length) ValueHelper.error("Index " + col + " out of bounds for matrix width " + values.length)
        if (row <= 0 || row > values(col.toInt - 1).length) ValueHelper.error("Index " + row + " out of bounds for matrix height " + values.length)
        values(col.toInt - 1)(row.toInt - 1)
      }
    } else {
      MathError("Matrices can only be applied to two arguments")
    }
  }
}

trait MathFunction extends MathValue {
  def string(calc: Calculator): String
}

object MathError {
  def apply(msg: String): MathError = MathError(msg, Vector())
}

object MathNumber {
  val Zero: MathNumber = MathNumber(BigComplex.ZERO)
  val One: MathNumber = MathNumber(BigComplex.ONE)
  def apply(real: BigDecimal): MathNumber = MathNumber(BigComplex.valueOf(real.bigDecimal))
  def apply(real: BigDecimal, imag: BigDecimal): MathNumber = MathNumber(BigComplex.valueOf(real.bigDecimal, imag.bigDecimal))
}

object MathBoolean {
  def apply(value: Boolean): MathValue = if (value) MathTrue else MathFalse
  def unapply(value: MathValue): Option[Boolean] = value match {
    case MathTrue => Some(true)
    case MathFalse => Some(false)
    case _ => None
  }
}

object MathNumeric {
  def unapply(value: MathValue): Option[BigComplex] = value match {
    case MathTrue => Some(BigComplex.ONE)
    case MathFalse => Some(BigComplex.ZERO)
    case MathNumber(num) => Some(num)
    case _ => None
  }
}

object MathRealNumeric {
  def unapply(value: MathValue): Option[BigDecimal] = value match {
    case MathNumeric(num) if num.isReal => Some(num.re)
    case _ => None
  }
}

object MathComplexNumeric {
  def unapply(value: MathValue): Option[(BigDecimal, BigDecimal)] = value match {
    case MathNumeric(num) => Some((num.re, num.im))
    case _ => None
  }
}

object MathVector {
  def apply(values: Vector[MathValue]): MathValue = MathMatrix(Vector(values))
  def unapply(value: MathValue): Option[Vector[MathValue]] = value match {
    case MathMatrix(Vector(values)) => Some(values)
    case _ => None
  }
}

