package tuxcalculator.core.math

import tuxcalculator.core.math.algebra.Algebra

object GreatestCommonDivisor {
  
  def gcd[T](ring: Algebra.ModularRing[T])(a: T, b: T): Result[T] = {
    var rl: T = a
    var sl: T = ring.one
    var tl: T = ring.zero
    var r: T = b
    var s: T = ring.zero
    var t: T = ring.one
    while (r != ring.zero) {
      val (q, rn) = ring.divMod(rl, r)
      val sn = ring.subtract(sl, ring.multiply(q, s))
      val tn = ring.subtract(tl, ring.multiply(q, t))
      rl = r; r = rn
      sl = s; s = sn
      tl = t; t = tn
    }
    Result(rl, sl, tl)
  }
  
  case class Result[T](gcd: T, bezout1: T, bezout2: T)
}
