package tuxcalculator.core.special

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.value._

object ListOperators {
  
  object Fold extends CalculatorSpecial.SimpleFunction("fold", 3) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      val list: Vector[MathValue] = ValueHelper.list(args(0))
      val start: MathValue = ValueHelper.get(args(1))
      val combine: MathValue = ValueHelper.get(args(2))
      list.foldLeft(start)((c, e) => combine.applyTo(calc, Vector(c, e)))
    }
  }
  
  object Len extends CalculatorSpecial.SimpleFunction("len", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args.head) match {
      case MathList(values) => MathNumber(values.length)
      case MathVector(values) => MathNumber(values.length)
      case _ => MathError("Can't get length of: " + calc.format(args.head))
    }}
  }
  
  object Rev extends CalculatorSpecial.SimpleFunction("rev", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args.head) match {
      case MathList(values) => MathList(values.reverse)
      case MathVector(values) => MathVector(values.reverse)
      case _ => MathError("Can't reverse: " + calc.format(args.head))
    }}
  }
  
  object Car extends CalculatorSpecial.SimpleFunction("car", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args.head) match {
      case MathList(Vector()) => MathError("car/head of empty list")
      case MathList(values) => values.head
      case MathVector(Vector()) => MathError("car/head of empty vector")
      case MathVector(values) => values.head
      case _ => MathError("Can't get car/head of: " + calc.format(args.head))
    }}
  }
  
  object Cdr extends CalculatorSpecial.SimpleFunction("cdr", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args.head) match {
      case MathList(Vector()) => MathError("cdr/tail of empty list")
      case MathList(values) => MathList(values.tail)
      case MathVector(Vector()) => MathError("cdr/tail of empty vector")
      case MathVector(Vector(_)) => MathError("cdr/tail of single-element vector is not defined") // Empty matrices can't exist
      case MathVector(values) => MathVector(values.tail)
      case _ => MathError("Can't get cdr/tail of: " + calc.format(args.head))
    }}
  }

  object Map extends CalculatorSpecial.SimpleFunction("map", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args(0)) match {
      case MathList(values) =>
        val func = ValueHelper.get(args(1))
        MathList(values.map(v => func.applyTo(calc, Vector(v))))
      case MathVector(values) =>
        val func = ValueHelper.get(args(1))
        MathVector(values.map(v => func.applyTo(calc, Vector(v))))
      case _ => MathError("Can't get tail of: " + calc.format(args.head))
    }}
  }

  object Zip extends CalculatorSpecial.SimpleFunction("zip", 3) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {(ValueHelper.get(args(0)), ValueHelper.get(args(1))) match {
      case (MathList(a), MathList(b)) =>
        val func = ValueHelper.get(args(2))
        MathList((a zip b).map(e => func.applyTo(calc, Vector(e._1, e._2))))
      case (MathVector(a), MathVector(b)) =>
        val func = ValueHelper.get(args(2))
        MathVector((a zip b).map(e => func.applyTo(calc, Vector(e._1, e._2))))
      case _ => MathError("Can't zip: " + calc.format(args.head))
    }}
  }
  
  object Filter extends CalculatorSpecial.SimpleFunction("filter", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args(0)) match {
      case MathList(values) =>
        val func = ValueHelper.get(args(1))
        MathList(values.zipWithIndex.filter(e => ValueHelper.boolean(func.applyTo(calc, Vector(e._1, MathNumber(e._2))))).map(_._1))
      case _ => MathError("Can't filter: " + calc.format(args.head))
    }
  }}
  
  object Idx extends CalculatorSpecial.SimpleFunction("idx", 3) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args(0)) match {
      case MathList(values) =>
        val from: BigInt = ValueHelper.realInt(args(1))
        if (from < 0) ValueHelper.error("Negative search index: " + from)
        if (from.isValidInt) {
          val fromIdx: Int = from.toInt
          if (fromIdx >= values.size) {
            MathNumber(-1)
          } else {
            val func = ValueHelper.get(args(2))
            MathNumber(values.indexWhere(value => ValueHelper.boolean(func.applyTo(calc, Vector(value))), fromIdx))
          }
        } else {
          MathNumber(-1)
        }
      case _ => MathError("Can't index: " + calc.format(args.head))
    }
  }}
  
  object Sort extends CalculatorSpecial.SimpleFunction("sort", 2) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {ValueHelper.get(args(0)) match {
      case MathList(values) =>
        val func = ValueHelper.get(args(1))
        MathList(values.sorted(Ordering.fromLessThan((a: MathValue, b: MathValue) => ValueHelper.boolean(func.applyTo(calc, Vector(a, b))))))
      case _ => MathError("Can't sort: " + calc.format(args.head))
    }
  }}
  
  object Fill extends CalculatorSpecial.Function("fill") {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) { args.size match {
      case 2 =>
        val max: BigInt = ValueHelper.realInt(args(0))
        if (max < 0) ValueHelper.error("Upper bound is negative: " + max)
        if (!max.isValidInt) ValueHelper.error("List too long: " + max)
        
        val func: MathValue = ValueHelper.get(args(1))
        MathList((0 until max.toInt).map(idx => func.applyTo(calc, Vector(MathNumber(idx)))).toVector)
      case 3 =>
        val maxHt: BigInt = ValueHelper.realInt(args(0))
        if (maxHt <= 0) ValueHelper.error("Matrix height is non-positive: " + maxHt)
        if (!maxHt.isValidInt) ValueHelper.error("Matrix too tall: " + maxHt)
        
        val maxWd: BigInt = ValueHelper.realInt(args(1))
        if (maxWd <= 0) ValueHelper.error("Matrix width is non-positive: " + maxWd)
        if (!maxWd.isValidInt) ValueHelper.error("Matrix too wide: " + maxWd)
        
        val func: MathValue = ValueHelper.get(args(2))
        MathMatrix((1 to maxWd.toInt).map(col => (1 to maxHt.toInt).map(row => func.applyTo(calc, Vector(MathNumber(row), MathNumber(col)))).toVector).toVector)
      case s => MathError("#" + name + " is only defined for 2 or 3 arguments, got " + s + ".")
    }}
  }
}
