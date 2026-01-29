package tuxcalculator.core.value

import ch.obermuhlner.math.big.BigComplex
import tuxcalculator.core.Calculator
import tuxcalculator.core.util.Util

// All arguments must always be normalized (the last element may not be zero)
object PolynomialOps {
  
  def add(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): MathValue = MathPolynomial(doAdd(calc, pol1, pol2))
  def sub(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): MathValue = MathPolynomial(doSub(calc, pol1, pol2))
  def mul(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): MathValue = MathPolynomial(doMul(calc, pol1, pol2))
  def div(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): MathValue = doDivMod(calc, pol1, pol2) match {
    case Left((result, Vector())) => MathPolynomial(result)
    case Left(_) => MathError("Polynomials are not divisible: (" + calc.format(MathPolynomial(pol1)) + ") / (" + calc.format(MathPolynomial(pol2)) + ")" )
    case Right(err) => MathError(err)
  }
  def truncatingDiv(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): MathValue = doDivMod(calc, pol1, pol2) match {
    case Left((result, _)) => MathPolynomial(result)
    case Right(err) => MathError(err)
  }
  def mod(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): MathValue = doDivMod(calc, pol1, pol2) match {
    case Left((_, result)) => MathPolynomial(result)
    case Right(err) => MathError(err)
  }

  def doAdd(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): Vector[MathNumber] = {
    val coefficients: Seq[BigComplex] = for (idx <- 0 until (pol1.length max pol2.length))
      yield coefficient(pol1, idx).add(coefficient(pol2, idx), calc.mathContext)
    normalize(coefficients.map(MathNumber.apply).toVector)
  }
  
  def doSub(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): Vector[MathNumber] = {
    val coefficients: Seq[BigComplex] = for (idx <- 0 until (pol1.length max pol2.length))
      yield coefficient(pol1, idx).subtract(coefficient(pol2, idx), calc.mathContext)
    normalize(coefficients.map(MathNumber.apply).toVector)
  }
  
  def doMul(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): Vector[MathNumber] = {
    if (pol1.isEmpty || pol2.isEmpty) return Vector()
    val coefficients: Seq[BigComplex] = for (idx <- 0 until (pol1.length + pol2.length - 1)) yield {
      val terms: Seq[BigComplex] = for (idx1 <- 0 to idx; idx2 = idx - idx1 if pol1.indices.contains(idx1) && pol2.indices.contains(idx2))
        yield coefficient(pol1, idx1).multiply(coefficient(pol2, idx2), calc.mathContext)
      terms.foldLeft(BigComplex.ZERO)((a, b) => a.add(b, calc.mathContext))
    }
    normalize(coefficients.map(MathNumber.apply).toVector)
  }
  
  def doDivMod(calc: Calculator, pol1: Vector[MathNumber], pol2: Vector[MathNumber]): Either[(Vector[MathNumber], Vector[MathNumber]), String] = {
    // There will never be a divide by zero if our arguments are normalized.
    def divNum(a: MathNumber, b: MathNumber): MathNumber = (a, b) match {
      case (MathRealNumeric(n1), MathRealNumeric(n2)) => MathNumber(n1.bigDecimal.divide(n2.bigDecimal, calc.mathContext))
      case (n1, MathRealNumeric(n2)) => MathNumber(n1.num.divide(n2.bigDecimal, calc.mathContext))
      case (n1, n2) => MathNumber(n1.num.divide(n2.num, calc.mathContext))
    }
    
    if (pol2.isEmpty) return Right("Division by zero")
    if (pol1.isEmpty) return Left((Vector(), Vector()))
    if (pol1.length < pol2.length) return Left((Vector(), pol1))
    val newHighestExp = pol1.length - pol2.length
    val firstTermPol: Vector[MathNumber] = normalize(Vector.fill(newHighestExp)(MathNumber.Zero) ++ Vector(divNum(pol1.last, pol2.last)))
    val fullSubtractPol = doMul(calc, firstTermPol, pol2)
    val remainderToDivideUnsafe = doSub(calc, pol1, fullSubtractPol)
    
    // Safe-guard against infinite loops because the polynomial does not shrink
    val remainderToDivide = doSub(calc, pol1, fullSubtractPol) match {
      case Vector() => Vector()
      case rem =>
        val normScale: Int = Util.safeStripTrailingZeros(remainderToDivideUnsafe.last.num.absSquare(calc.mathContext)).scale() / 2
        if (normScale > calc.precision * 100) rem.init else rem
    }

    doDivMod(calc, remainderToDivide, pol2) match {
      case Left((quot, rem)) => Left((doAdd(calc, firstTermPol, quot), rem))
      case Right(err) => Right(err)
    }
  }
  
  def raise(calc: Calculator, pol: Vector[MathNumber], exp: Int): MathValue = {
    if (exp == 0) {
      if (pol.isEmpty) MathError("0^0 is undefined") else MathNumber.One
    } else if (exp > 0) {
      def doRaise(thePol: Vector[MathNumber], theExp: Int): Vector[MathNumber] = theExp match {
        case 1 => thePol
        case 2 => doMul(calc, thePol, thePol)
        case n if n % 2 == 0 =>
          val halfRaised = doRaise(thePol, n / 2)
          doMul(calc, halfRaised, halfRaised)
        case n =>
          val subRaised = doRaise(thePol, n - 1)
          doMul(calc, subRaised, thePol)
      }
      MathPolynomial(doRaise(pol, exp))
    } else if (pol.size == 1) {
      NumberHelper.pow(pol(0), MathNumber(exp))
    } else {
      MathError("Negative polynomial power.")
    }
  }

  def normalize(pol: Vector[MathNumber]): Vector[MathNumber] = pol.reverse.dropWhile(num => num.num == BigComplex.ZERO).reverse
  private def coefficient(pol: Vector[MathNumber], at: Int): BigComplex = if (pol.indices.contains(at)) pol(at).num else BigComplex.ZERO
}
