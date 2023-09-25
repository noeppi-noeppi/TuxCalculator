package tuxcalculator.core.data

import tuxcalculator.core.Calculator
import tuxcalculator.core.value.{MathError, MathValue, MathVoid, ValueHelper}

import scala.collection.mutable

sealed trait CalculatorProperty[T] {
  def from(calc: Calculator, value: MathValue): Either[T, MathValue]
  def validate(value: T): Either[T, String] = Left(value)
}

class CalculatorProperties(private val calc: Calculator, val onChange: () => Unit) {
  
  private[this] val values: mutable.Map[CalculatorProperty[_], Any] = mutable.Map(
    CalculatorProperties.Precision -> 16,
    CalculatorProperties.Output -> 0,
    CalculatorProperties.Truncate -> 0,
    CalculatorProperties.Eager -> false
  )
  
  def apply[T](property: CalculatorProperty[T]): T = values(property).asInstanceOf[T]
  
  def set(name: String, value: MathValue): MathValue = {
    def doSet[T](property: CalculatorProperty[T], value: MathValue): MathValue = property.from(calc, value) match {
      case Left(theValue) => set(property, theValue)
      case Right(errVal) => errVal
    }
    
    CalculatorProperties(name) match {
      case Left(property) => doSet(property, value)
      case Right(msg) => MathError(msg)
    }
  }
  
  def set[T](property: CalculatorProperty[T], value: T): MathValue = property.validate(value) match {
    case Left(theValue) =>
      values(property) = theValue
      onChange()
      MathVoid
    case Right(msg) => MathError(msg)
  }
}

object CalculatorProperties {
  
  def apply(name: String): Either[CalculatorProperty[_], String] = name match {
    case "precision" => Left(Precision)
    case "output" => Left(Output)
    case "truncate" => Left(Truncate)
    case "eager" => Left(Eager)
    case _ => Right("Unknown calculator property: '" + name + "'")
  }
  
  case object Precision extends CalculatorProperty[Int] {
    override def from(calc: Calculator, value: MathValue): Either[Int, MathValue] = ValueHelper.make(calc) { ValueHelper.realInt(value).toInt }
    override def validate(value: Int): Either[Int, String] = value match {
      case _ if value <= 0 => Right("Precision must be positive")
      case _ => Left(value)
    }
  }
  
  case object Output extends CalculatorProperty[Int] {
    override def from(calc: Calculator, value: MathValue): Either[Int, MathValue] = ValueHelper.make(calc) { ValueHelper.realInt(value).toInt }
    override def validate(value: Int): Either[Int, String] = value match {
      case _ if value < 0 => Right("Output precision must be non-negative")
      case _ => Left(value)
    }
  }
  
  case object Truncate extends CalculatorProperty[Int] {
    override def from(calc: Calculator, value: MathValue): Either[Int, MathValue] = ValueHelper.make(calc) { ValueHelper.realInt(value).toInt }
    override def validate(value: Int): Either[Int, String] = value match {
      case _ if value < 0 => Right("Truncation can't be negative")
      case _ => Left(value)
    }
  }
  
  case object Eager extends CalculatorProperty[Boolean] {
    override def from(calc: Calculator, value: MathValue): Either[Boolean, MathValue] = ValueHelper.make(calc) { ValueHelper.boolean(value) }
  }
}
