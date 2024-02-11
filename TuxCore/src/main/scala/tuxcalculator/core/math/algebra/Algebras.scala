package tuxcalculator.core.math.algebra

import Algebra._
import ch.obermuhlner.math.big.BigComplex
import tuxcalculator.core.Calculator
import tuxcalculator.core.value.{MathNumber, PolynomialOps}

import java.math.{BigDecimal => BigDec}

object Algebras {
  
  val integers: ModularRing[BigInt] = new ModularRing[BigInt] {
    override def zero: BigInt = BigInt(0)
    override def one: BigInt = BigInt(1)
    override def negate(a: BigInt): BigInt = -a
    override def add(a: BigInt, b: BigInt): BigInt = a + b
    override def subtract(a: BigInt, b: BigInt): BigInt = a - b
    override def multiply(a: BigInt, b: BigInt): BigInt = a * b
    override def divMod(a: BigInt, b: BigInt): (BigInt, BigInt) = a /% b
  }

  def reals(calc: Calculator): OrderedField[BigDec] = new OrderedField[BigDec] {
    override def zero: BigDec = BigDec.ZERO
    override def one: BigDec = BigDec.ONE
    override def negate(a: BigDec): BigDec = a.negate(calc.mathContext)
    override def add(a: BigDec, b: BigDec): BigDec = a.add(b, calc.mathContext)
    override def subtract(a: BigDec, b: BigDec): BigDec = a.subtract(b, calc.mathContext)
    override def invert(a: BigDec): BigDec = BigDec.ONE.divide(a, calc.mathContext)
    override def multiply(a: BigDec, b: BigDec): BigDec = a.multiply(b, calc.mathContext)
    override def divide(a: BigDec, b: BigDec): BigDec = a.divide(b, calc.mathContext)
    override val ordering: Ordering[BigDec] = implicitly[Ordering[BigDec]]
    override def compare(a: BigDec, b: BigDec): Int = a.compareTo(b)
  }
  
  def complex(calc: Calculator): Field[BigComplex] = new Field[BigComplex] {
    override def zero: BigComplex = BigComplex.ZERO
    override def one: BigComplex = BigComplex.ONE
    override def negate(a: BigComplex): BigComplex = a.negate().round(calc.mathContext)
    override def add(a: BigComplex, b: BigComplex): BigComplex = a.add(b, calc.mathContext)
    override def subtract(a: BigComplex, b: BigComplex): BigComplex = a.subtract(b, calc.mathContext)
    override def invert(a: BigComplex): BigComplex = BigComplex.ONE.divide(a, calc.mathContext)
    override def multiply(a: BigComplex, b: BigComplex): BigComplex = a.multiply(b, calc.mathContext)
    override def divide(a: BigComplex, b: BigComplex): BigComplex = a.divide(b, calc.mathContext)
  }
  
  def polynomials(calc: Calculator): ModularRing[Vector[MathNumber]] = new ModularRing[Vector[MathNumber]] {
    override def zero: Vector[MathNumber] = Vector()
    override def one: Vector[MathNumber] = Vector(MathNumber.One)
    override def negate(a: Vector[MathNumber]): Vector[MathNumber] = PolynomialOps.doSub(calc, zero, PolynomialOps.normalize(a))
    override def add(a: Vector[MathNumber], b: Vector[MathNumber]): Vector[MathNumber] = PolynomialOps.doAdd(calc, PolynomialOps.normalize(a), PolynomialOps.normalize(b))
    override def subtract(a: Vector[MathNumber], b: Vector[MathNumber]): Vector[MathNumber] = PolynomialOps.doSub(calc, PolynomialOps.normalize(a), PolynomialOps.normalize(b))
    override def multiply(a: Vector[MathNumber], b: Vector[MathNumber]): Vector[MathNumber] = PolynomialOps.doMul(calc, PolynomialOps.normalize(a), PolynomialOps.normalize(b))
    override def divMod(a: Vector[MathNumber], b: Vector[MathNumber]): (Vector[MathNumber], Vector[MathNumber]) = PolynomialOps.doDivMod(calc, PolynomialOps.normalize(a), PolynomialOps.normalize(b)) match {
      case Left(result) => result
      case Right(error) => throw new ArithmeticException(error)
    }
  }
}
