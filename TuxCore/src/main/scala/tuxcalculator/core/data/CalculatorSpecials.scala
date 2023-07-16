package tuxcalculator.core.data

import tuxcalculator.core.Calculator
import tuxcalculator.core.special.{BasicOperators, BuiltinFunctions, Constants, ListOperators, LogicOperators, MatrixOperators, RelOperators, TestOperators}
import tuxcalculator.core.value.{MathError, MathFunction, MathValue}

import java.math.MathContext
import scala.collection.mutable

trait PropertyAccess {
  def precision: Int
  def mathContext: MathContext
  def properties: CalculatorProperties
}

class CalculatorSpecials(private val calc: PropertyAccess) {

  private val specials: Map[String, CalculatorSpecial] = Seq(
    Constants.Void,
    Constants.True,
    Constants.False,
    Constants.Pi,
    Constants.Euler,
    Constants.ImaginaryUnit,
    BasicOperators.Re,
    BasicOperators.Im,
    BasicOperators.Add,
    BasicOperators.Sub,
    BasicOperators.Mul,
    BasicOperators.Div,
    BasicOperators.Mod,
    BasicOperators.Pow,
    BasicOperators.Polar,
    TestOperators.IsList,
    TestOperators.IsMatrix,
    TestOperators.IsVector,
    TestOperators.IsBoolean,
    TestOperators.IsReal,
    ListOperators.Fold,
    ListOperators.Len,
    ListOperators.Rev,
    ListOperators.Car,
    ListOperators.Cdr,
    ListOperators.Filter,
    ListOperators.Map,
    ListOperators.Zip,
    ListOperators.Idx,
    ListOperators.Sort,
    ListOperators.Fill,
    MatrixOperators.Wd,
    MatrixOperators.Ht,
    MatrixOperators.Adj,
    MatrixOperators.Det,
    RelOperators.Eq,
    RelOperators.Lt,
    BuiltinFunctions.Abs,
    BuiltinFunctions.Gamma,
    BuiltinFunctions.Fib,
    BuiltinFunctions.Ln,
    BuiltinFunctions.W,
    BuiltinFunctions.Li,
    BuiltinFunctions.Sin,
    BuiltinFunctions.Sinh,
    BuiltinFunctions.Cosh,
    BuiltinFunctions.Asin,
    BuiltinFunctions.Atan,
    BuiltinFunctions.If,
    BuiltinFunctions.Gcd,
    BuiltinFunctions.Rd,
    BuiltinFunctions.Cl,
    BuiltinFunctions.Fl,
    BuiltinFunctions.Agm,
    LogicOperators.And,
    LogicOperators.Or,
    LogicOperators.Xor
  ).map(special => special.name -> special).toMap
  
  val keys: Set[String] = specials.keySet
  
  private val constant: mutable.Map[CalculatorSpecial, MathValue] = mutable.Map()
  private val dynamic: mutable.Map[CalculatorSpecial, MathValue] = mutable.Map()
  
  def apply(name: String): MathValue = specials.get(name) match {
    case Some(special: CalculatorSpecial.Constant) => constant.getOrElseUpdate(special, special.value)
    case Some(special: CalculatorSpecial.Dynamic) => dynamic.getOrElseUpdate(special, special.value(calc))
    case None => MathError("Unknown special: '" + name + "'")
  }

  def propertyChange(): Unit = dynamic.clear()
}

sealed trait CalculatorSpecial {
  def name: String
}
object CalculatorSpecial {
  abstract class Constant(val name: String) extends CalculatorSpecial {
    def value: MathValue
  }
  abstract class Dynamic(val name: String) extends CalculatorSpecial {
    def value(calc: PropertyAccess): MathValue
  }
  abstract class Function(name: String) extends Constant(name) {
    override def value: MathValue = new SpecialFunction(name) {
      override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = result(calc, args)
    }
    protected def result(calc: Calculator, args: Vector[MathValue]): MathValue
  }
  abstract class SimpleFunction(name: String, argNum: Int) extends Constant(name) {
    override def value: MathValue = new SpecialFunction(name) {
      override def applyTo(calc: Calculator, args: Vector[MathValue]): MathValue = args.size match {
        case s if s == argNum => result(calc, args)
        case s => MathError("#" + this.name + " is only defined for " + argNum + " arguments, got " + s + ".")
      }
    }
    protected def result(calc: Calculator, args: Vector[MathValue]): MathValue
  }
}

sealed abstract class SpecialFunction(val name: String) extends MathFunction {
  override def string(calc: Calculator): String = "#" + name
}
