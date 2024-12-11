package tuxcalculator.core.data

import tuxcalculator.core.Calculator
import tuxcalculator.core.value.{MathError, MathValue, MathVoid, ValueHelper}

import java.text.Normalizer
import scala.collection.mutable

sealed trait CalculatorProperty[T] {
  def default: T
  def from(calc: Calculator, value: MathValue): Either[T, MathValue]
  def validate(value: T): Either[T, String] = Left(value)
}

class CalculatorProperties(private val calc: Calculator, val onChange: () => Unit) {
  
  private[this] val values: mutable.Map[CalculatorProperty[_], Any] = mutable.Map()
  
  def apply[T](property: CalculatorProperty[T]): T = values.getOrElseUpdate(property, property.default).asInstanceOf[T]
  
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
  
  // New calculator properties must be added to this set, to the apply method and be stored and loaded in FormatIO
  def allProperties: Set[String] = Set[String](
    "precision", "output", "truncate", "eager", "normalization", "highlight", "polar", "autoref"
  )
  
  def apply(name: String): Either[CalculatorProperty[_], String] = name match {
    case "precision" => Left(Precision)
    case "output" => Left(Output)
    case "truncate" => Left(Truncate)
    case "eager" => Left(Eager)
    case "normalization" => Left(Normalization)
    case "highlight" => Left(Highlight)
    case "polar" => Left(Polar)
    case "autoref" => Left(Autoref)
    case _ => Right("Unknown calculator property: '" + name + "'")
  }
  
  case object Precision extends CalculatorProperty[Int] {
    override def default: Int = 16
    override def from(calc: Calculator, value: MathValue): Either[Int, MathValue] = ValueHelper.make(calc) { ValueHelper.realInt(value).toInt }
    override def validate(value: Int): Either[Int, String] = value match {
      case _ if value <= 0 => Right("Precision must be positive")
      case _ => Left(value)
    }
  }
  
  case object Output extends CalculatorProperty[Int] {
    override def default: Int = 0
    override def from(calc: Calculator, value: MathValue): Either[Int, MathValue] = ValueHelper.make(calc) { ValueHelper.realInt(value).toInt }
    override def validate(value: Int): Either[Int, String] = value match {
      case _ if value < 0 => Right("Output precision must be non-negative")
      case _ => Left(value)
    }
  }
  
  case object Truncate extends CalculatorProperty[Int] {
    override def default: Int = 0
    override def from(calc: Calculator, value: MathValue): Either[Int, MathValue] = ValueHelper.make(calc) { ValueHelper.realInt(value).toInt }
    override def validate(value: Int): Either[Int, String] = value match {
      case _ if value < 0 => Right("Truncation can't be negative")
      case _ => Left(value)
    }
  }
  
  case object Eager extends CalculatorProperty[Boolean] {
    override def default: Boolean = false
    override def from(calc: Calculator, value: MathValue): Either[Boolean, MathValue] = ValueHelper.make(calc) { ValueHelper.boolean(value) }
  }
  
  case object Normalization extends CalculatorProperty[Option[Normalizer.Form]] {
    override def default: Option[Normalizer.Form] = None
    override def from(calc: Calculator, value: MathValue): Either[Option[Normalizer.Form], MathValue] = value match {
      case MathError("", _) => Left(None)
      case MathError("nfc", _) => Left(Some(Normalizer.Form.NFC))
      case MathError("nfd", _) => Left(Some(Normalizer.Form.NFD))
      case MathError("nfkc", _) => Left(Some(Normalizer.Form.NFKC))
      case MathError("nfkd", _) => Left(Some(Normalizer.Form.NFKD))
      case MathError(str, _) => Right(MathError("Invalid value for normalization: " + str))
      case _ => Right(MathError("Invalid value for normalization: " + calc.format(value)))
    }
  }
  
  case object Highlight extends CalculatorProperty[Boolean] {
    override def default: Boolean = false
    override def from(calc: Calculator, value: MathValue): Either[Boolean, MathValue] = ValueHelper.make(calc) { ValueHelper.boolean(value) }
  }
  
  case object Polar extends CalculatorProperty[PolarType] {
    override def default: PolarType = PolarType.None
    override def from(calc: Calculator, value: MathValue): Either[PolarType, MathValue] = value match {
      case MathError("", _) => Left(PolarType.None)
      case MathError("rad", _) => Left(PolarType.Radians)
      case MathError("deg", _) => Left(PolarType.Degrees)
      case MathError(str, _) => Right(MathError("Invalid value for polar formatting: " + str))
      case _ => ValueHelper.make(calc) {
        if (ValueHelper.boolean(value)) PolarType.Radians else PolarType.None
      }
    }
  }
  type PolarType = PolarType.Value
  object PolarType extends Enumeration {
    val None: PolarType = Value("none")
    val Radians: PolarType = Value("radians")
    val Degrees: PolarType = Value("degrees")
  }
  
  case object Autoref extends CalculatorProperty[Boolean] {
    override def default: Boolean = false
    override def from(calc: Calculator, value: MathValue): Either[Boolean, MathValue] = ValueHelper.make(calc) { ValueHelper.boolean(value) }
  }
}
