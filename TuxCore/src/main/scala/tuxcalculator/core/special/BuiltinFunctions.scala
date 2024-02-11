package tuxcalculator.core.special

import ch.obermuhlner.math.big.{BigComplex, BigComplexMath, BigDecimalMath}
import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.math.algebra.Algebras
import tuxcalculator.core.math.{ArithmeticGeometricMean, GreatestCommonDivisor, IncompleteGamma, LogarithmicIntegral, ProductLog}
import tuxcalculator.core.value.{MathComplexNumeric, MathError, MathList, MathNumber, MathNumeric, MathPolynomial, MathPolynomic, MathRealNumeric, MathValue, ValueHelper}

import java.math.MathContext
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

object BuiltinFunctions {

  object Abs extends CalculatorSpecial.SimpleFunction("abs", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      ValueHelper.get(args(0)) match {
        case MathRealNumeric(num) => MathNumber(num.bigDecimal.abs(calc.mathContext))
        case MathNumeric(num) => MathNumber(num.abs(calc.mathContext))
        case v => MathError("abs(" + calc.format(v) + ") is undefined.")
      }
    }
  }
  
  object Gamma extends CalculatorSpecial.SimpleFunction("gamma", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {(ValueHelper.get(args(0)), ValueHelper.get(args(1))) match {
      case (MathNumeric(p), MathNumeric(x)) => MathNumber(IncompleteGamma.gamma(p, x, calc.mathContext))
      case (p, x) => MathError("Gammma(" + calc.format(p) + ", " + calc.format(x) + ") is undefined.")
    }}
  }
  
  object Bernoulli extends CalculatorSpecial.SimpleFunction("bernoulli", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      val n = ValueHelper.realInt(args(0))
      if (!n.isValidInt) ValueHelper.error("Bernoulli index too large: " + n)
      if (n < 0) ValueHelper.error("Negative bernoulli index: " + n)
      MathNumber(BigDecimalMath.bernoulli(n.toInt, calc.mathContext))
    }
  }
  
  object Ln extends CalculatorSpecial.SimpleFunction("ln", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args.head) match {
      case MathNumeric(num) if num == BigComplex.ZERO => MathError("ln(0) is undefined.")
      case MathNumeric(num) => MathNumber(BigComplexMath.log(num, calc.mathContext))
      case v => MathError("ln(" + calc.format(v) + ") is undefined.")
    }}
  }
  
  object W extends CalculatorSpecial.SimpleFunction("w", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args.head) match {
      case MathNumeric(num) if num == BigComplex.ZERO => MathNumber(0)
      case MathNumeric(num) => MathNumber(ProductLog.productLog(num, calc.mathContext))
      case v => MathError("W(" + calc.format(v) + ") is undefined.")
    }}
  }
  
  object Li extends CalculatorSpecial.SimpleFunction("li", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args.head) match {
      case MathNumeric(num) if num == BigComplex.ONE => MathError("li(1) is undefined.")
      case MathNumeric(num) => MathNumber(LogarithmicIntegral.logarithmicIntegral(num, calc.mathContext))
      case v => MathError("li(" + calc.format(v) + ") is undefined.")
    }}
  }
  
  abstract class AngleFunc(name: String, rFunc: (java.math.BigDecimal, MathContext) => java.math.BigDecimal, cFunc: Option[(BigComplex, MathContext) => BigComplex]) extends CalculatorSpecial.SimpleFunction(name, 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = try {
      ValueHelper.run(calc) {ValueHelper.get(args.head) match {
        case MathRealNumeric(num) => MathNumber(rFunc(num.bigDecimal, calc.mathContext))
        case MathNumber(num) if cFunc.isDefined => MathNumber(cFunc.get(num, calc.mathContext))
        case v => MathError(name + "(" + calc.format(v) + ") is undefined")
      }}
    } catch {
      case e: ArithmeticException => MathError(e.getMessage)
    }
  }
  
  object Sin extends AngleFunc("sin", BigDecimalMath.sin, Some(BigComplexMath.sin))
  object Sinh extends AngleFunc("sinh", BigDecimalMath.sinh, None)
  object Cosh extends AngleFunc("cosh", BigDecimalMath.cosh, None)
  object Asin extends AngleFunc("asin", BigDecimalMath.asin, Some(BigComplexMath.asin))

  object Atan extends CalculatorSpecial.SimpleFunction("atan", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {(ValueHelper.get(args(0)), ValueHelper.get(args(1))) match {
      case (MathRealNumeric(y), MathRealNumeric(x)) if x == 1 => MathNumber(BigDecimalMath.atan(y.bigDecimal, calc.mathContext))
      case (MathNumeric(y), MathRealNumeric(x)) if x == 1 => MathNumber(BigComplexMath.atan(y, calc.mathContext))
      case (MathRealNumeric(y), MathRealNumeric(x)) => MathNumber(BigDecimalMath.atan2(y.bigDecimal, x.bigDecimal, calc.mathContext))
      case (y, x) => MathError("atan(" + calc.format(y) + "," + calc.format(x) + ") is undefined.")
    }}
  }
  
  object If extends CalculatorSpecial.SimpleFunction("if", 3) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      if (ValueHelper.boolean(args(0))) {
        args(1)
      } else {
        args(2)
      }
    }
  }
  
  object Gcd extends CalculatorSpecial.SimpleFunction("gcd", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      (args(0), args(1)) match {
        case (n1 @ MathNumeric(_), n2 @ MathNumeric(_)) =>
          // If both arguments are degree 0 polynomials (numbers), compute numeric gcd.
          val a = ValueHelper.realInt(n1)
          val b = ValueHelper.realInt(n2)
          MathNumber(BigDecimal(a.abs gcd b.abs))
        case (MathPolynomic(c1), MathPolynomic(c2)) =>
          val result = GreatestCommonDivisor.gcd(Algebras.polynomials(calc))(c1, c2)
          if (result.gcd.isEmpty) return MathPolynomial(result.gcd)
          val factor: BigComplex = result.gcd.last.num
          MathPolynomial(result.gcd.init.map(coefficient => MathNumber(coefficient.num.divide(factor, calc.mathContext))) ++ Vector(MathNumber.One))
        case (a, b) => MathError("Invalid gcd(" + calc.format(a) + ", " + calc.format(b) + ")")
      }
    }
  }
  
  object Bezout extends CalculatorSpecial.SimpleFunction("bezout", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      (args(0), args(1)) match {
        case (n1 @ MathNumeric(_), n2 @ MathNumeric(_)) =>
          // If both arguments are degree 0 polynomials (numbers), compute numeric gcd.
          val a = ValueHelper.realInt(n1)
          val b = ValueHelper.realInt(n2)
          val result = GreatestCommonDivisor.gcd(Algebras.integers)(a, b)
          MathList(Vector(MathNumber(BigDecimal(result.bezout1)), MathNumber(BigDecimal(result.bezout2))))
        case (MathPolynomic(c1), MathPolynomic(c2)) =>
          val result = GreatestCommonDivisor.gcd(Algebras.polynomials(calc))(c1, c2)
          if (result.gcd.isEmpty) return MathList(Vector(MathPolynomial(result.bezout1), MathPolynomial(result.bezout2)))
          val factor: BigComplex = result.gcd.last.num
          MathList(Vector(
            MathPolynomial(result.bezout1.map(coefficient => MathNumber(coefficient.num.divide(factor, calc.mathContext)))),
            MathPolynomial(result.bezout2.map(coefficient => MathNumber(coefficient.num.divide(factor, calc.mathContext))))
          ))
        case (a, b) => MathError("Invalid bezout(" + calc.format(a) + ", " + calc.format(b) + ")")
      }
    }
  }
  
  abstract class Rounding(name: String, mode: RoundingMode) extends CalculatorSpecial.SimpleFunction(name, 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      val precisionVal = ValueHelper.realInt(args(1))
      if (!precisionVal.isValidInt) ValueHelper.error("Invalid precision: " + precisionVal)
      ValueHelper.get(args(0)) match {
        case MathComplexNumeric(real, imag) => MathNumber(real.setScale(precisionVal.toInt, mode), imag.setScale(precisionVal.toInt, mode))
        case v => MathError("Can't " + name + ": " + calc.format(v))
      }
    }
  }
  
  object Rd extends Rounding("rd", RoundingMode.HALF_UP)
  object Cl extends Rounding("cl", RoundingMode.CEILING)
  object Fl extends Rounding("fl", RoundingMode.FLOOR)
  
  object Agm extends CalculatorSpecial.SimpleFunction("agm", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      (args(0), args(1)) match {
        case (MathNumeric(gm), MathNumeric(am)) => MathNumber(ArithmeticGeometricMean.agm(gm, am, calc.mathContext))
        case (gm, am) => MathError("Arithmetic-geometric mean is not defined for (" + calc.format(gm) + ", " + calc.format(am) + ")")
      }
    }
  }
}
