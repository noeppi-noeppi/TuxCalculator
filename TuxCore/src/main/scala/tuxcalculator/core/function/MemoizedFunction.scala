package tuxcalculator.core.function

import tuxcalculator.core.Calculator
import tuxcalculator.core.value.{MathFunction, MathValue}

import scala.collection.mutable
import scala.ref.SoftReference

class MemoizedFunction(val function: MathFunction) extends MathFunction {
  
  @transient
  private[this] var _cache: SoftReference[mutable.Map[Vector[MathValue], MathValue]] = SoftReference(mutable.Map())
  
  private def cache: mutable.Map[Vector[MathValue], MathValue] = _cache.get match {
    case Some(map) => map
    case None =>
      val map: mutable.Map[Vector[MathValue], MathValue] = mutable.Map()
      _cache = SoftReference(map)
      map
  }
  
  override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = cache.getOrElseUpdate(args, function.applyTo(calc, args))
  override def string(calc: Calculator): String = function.string(calc)
}
