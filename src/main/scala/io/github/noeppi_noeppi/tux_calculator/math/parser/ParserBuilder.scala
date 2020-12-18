package io.github.noeppi_noeppi.tux_calculator.math.parser

import scala.collection.mutable
import io.github.noeppi_noeppi.tux_calculator.math.MDefault.{absolute, acos, acosh, acot, acoth, acsc, acsch, and, asec, asech, asin, asinh, atan, atan2, atanh, call, cbrt, ceil, ceil2, clamp, cos, cosh, cot, coth, crossP, csc, csch, degree, degreeSign, dice, divide, dotP, e, equal, factorial, fc, fib, floor, floor2, fmap, foldl, foldla, foldr, foldra, foldv, foldva, fparams, gcd, gmean, greater, hmean, hypot, implic, inf, int, integ, knuth, lcm, len, list, list0, lmap, ln, log, log10, lower, max, mean, min, modulo, multiply, nCr, nPr, nand, negate, negateTilde, nimplic, nor, not, or, pi, pit, power, prime, probablePrime, prod, radian, rand, rand0, rand2, randchoice, randint, range, range2, reverse, rknuth, root, round, round2, sec, sech, seq, sgn, sin, sinh, slice, sqrt, subtract, sum, tan, tanh, vecA, vecAdd, vecMul, vecN, vecNorm, xor}
import io.github.noeppi_noeppi.tux_calculator.math.{Constant, DocumentationObject, MDefault, MFunction, Operator, PostfixUnary, Unary}

class ParserBuilder private() {

  private val const = mutable.ListBuffer[Constant]()
  private val func = mutable.ListBuffer[MFunction]()
  private val op = mutable.ListBuffer[Operator]()
  private val unary = mutable.ListBuffer[Unary]()
  private val postfixUnary = mutable.ListBuffer[PostfixUnary]()
  private val vars = mutable.Map[String, Double]()
  private val docs = mutable.Set[DocumentationObject]()

  def addDefault(): ParserBuilder = {
    const ++= List(pi, e, inf)
    func ++= List(sin, cos, tan, asin, acos, atan, sinh, cosh, tanh, sqrt, cbrt, ceil, floor, ln, log10,
      degree, radian, log, root, max, min, mean, clamp, absolute, round, round2, atan2, rand, rand2, randint,
      randchoice, knuth, sum, prod, foldl, foldv, fmap, call, fc, lcm, gcd, hypot, foldla, foldva,
      fparams, integ, gmean, hmean, floor2, ceil2, and, or, xor, not, int, foldr, foldra, rand0, cot, sec, csc,
      acot, asec, acsc, coth, sech, csch, nand, nor, implic, nimplic, dice, prime, probablePrime, rknuth, nPr,
      nCr, asinh, acosh, atanh, acoth, asech, acsch, pit, list0, list, lmap, len, range, range2, slice, reverse,
      sgn, dotP, crossP, vecN, vecA, vecNorm, vecAdd, vecMul, fib, seq)
    op ++= List(MDefault.add, subtract, multiply, divide, power, modulo, equal, lower, greater)
    unary ++= List(negate, negateTilde)
    postfixUnary ++= List(degreeSign, factorial)
    docs ++= MDefault.additionalDocumentation
    this
  }

  def addAll(oo: Iterable[Any]): ParserBuilder = { oo.foreach(o => add(o)); this}

  def add(o: Any): ParserBuilder = {o match {
    case x: Constant => const += x
    case x: MFunction => func += x
    case x: Operator => op += x
    case x: Unary => unary += x
    case x: PostfixUnary => postfixUnary += x
    case x: IterableOnce[_] => x.iterator.foreach(y => add(y))
    case (key: String, value: Double) => vars += key -> value
    case x: DocumentationObject => docs += x
    case x => throw new IllegalArgumentException("Could not add Object to ParserBuilder: " + x)
  };this}

  def build(): Parser = new Parser(const.toList, func.toList, op.toList, unary.toList, postfixUnary.toList, vars, docs.toSet, new FuncData)

  def extend(parser: Parser): Parser = new Parser(const.toList, func.toList, op.toList, unary.toList, postfixUnary.toList, vars, docs.toSet, parser.funcData)
}

object ParserBuilder {

  def apply(): ParserBuilder = new ParserBuilder()
}
