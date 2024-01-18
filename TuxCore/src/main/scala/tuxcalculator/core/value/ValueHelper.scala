package tuxcalculator.core.value

import ch.obermuhlner.math.big.BigComplex
import tuxcalculator.core.Calculator

object ValueHelper {

  class AbortError(val err: MathError) extends Exception
  class VoidError() extends Exception
  
  private val _calc: ThreadLocal[Calculator] = new ThreadLocal()
  def calc: Calculator = _calc.get() match {
    case null => throw new IllegalStateException("Wrong use of ValueHelper#run. This is a bug.")
    case c => c
  }

  def run(calc: Calculator)(code: => MathValue): MathValue = {
    val prev: Calculator = _calc.get()
    try {
      _calc.set(calc)
      code
    } catch {
      case e: AbortError => e.err
      case _: VoidError => MathVoid
    } finally {
      if (prev == null) _calc.remove() else _calc.set(prev)
    }
  }
  
  def make[T](calc: Calculator)(code: => T): Either[T, MathValue] = {
    val prev: Calculator = _calc.get()
    try {
      _calc.set(calc)
      Left(code)
    } catch {
      case e: AbortError => Right(e.err)
      case _: VoidError => Right(MathVoid)
    } finally {
      if (prev == null) _calc.remove() else _calc.set(prev)
    }
  }
  
  def get(value: MathValue): MathValue = value match {
    case err: MathError => throw new AbortError(err)
    case MathVoid => throw new VoidError
    case MathNumber(num) => MathNumber(num.round(calc.mathContext))
    case res => res
  }
  
  def list(value: MathValue): Vector[MathValue] = get(value) match {
    case MathList(values) => values
    case v => throw new AbortError(MathError("Expected a list, got: " + calc.format(v)))
  }
  
  def listOrVector(value: MathValue): Vector[MathValue] = get(value) match {
    case MathList(values) => values
    case MathVector(values) => values
    case v => throw new AbortError(MathError("Expected a list or vector, got: " + calc.format(v)))
  }
  
  def error(msg: String): Nothing = throw new AbortError(MathError(msg))
  def plainError(msg: String): Nothing = if (_calc.get() != null) error(msg) else throw new ArithmeticException(msg)

  def complex(value: MathValue): BigComplex = get(value.number(calc)) match {
    case MathNumeric(num) => num
    case v => throw new AbortError(MathError("Expected a number, got: " + calc.format(v)))
  }

  def real(value: MathValue): BigDecimal = get(value.number(calc)) match {
    case MathRealNumeric(real) => real.round(calc.mathContext)
    case v => throw new AbortError(MathError("Expected a real number, got: " + calc.format(v)))
  }
  
  def realInt(value: MathValue): BigInt = get(value.number(calc)) match {
    case MathRealNumeric(real) if real.isWhole => real.toBigInt
    case v => throw new AbortError(MathError("Expected a real integer, got: " + calc.format(v)))
  }
  
  def boolean(value: MathValue): Boolean = get(value) match {
    case MathTrue => true
    case MathFalse => false
    case MathNumber(num) => num != BigComplex.ZERO
    case MathList(Vector()) => false
    case MathMatrix(Vector()) => false
    case _ => true
  }
  
  def matrix(value: MathValue): MathMatrix = get(value) match {
    case m: MathMatrix => m
    case v => throw new AbortError(MathError("Expected a matrix, got: " + calc.format(v)))
  }
}
