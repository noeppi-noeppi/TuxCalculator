package io.github.noeppi_noeppi.tux_calculator.math

import java.math.MathContext
import io.github.noeppi_noeppi.tux_calculator.math.parser.{FuncData, Parser}

import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.RoundingMode
import scala.util.Random

object MDefault {

  object add extends Operator {
    override val name: String = "+"
    override val priority: Priority = Priority.ADDITIVE
    override val rightAssoc: Boolean = false
    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = op1 + op2

    override val name2: String = "Summe"
    override val doc: String = "Addiert zwei Summanden."
  }

  object subtract extends Operator {
    override val name: String = "-"
    override val priority: Priority = Priority.ADDITIVE
    override val rightAssoc: Boolean = false
    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = op1 - op2

    override val name2: String = "Differenz"
    override val doc: String = "Subtrahiert einen Wert von einem Anderen."
  }

  object multiply extends Operator {
    override val name: String = "*"
    override val priority: Priority = Priority.MULTIPLICATIVE
    override val rightAssoc: Boolean = false
    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = op1 * op2

    override val name2: String = "Produkt"
    override val doc: String = "Multipliziert zwei Werte."
  }

  object divide extends Operator {
    override val name: String = "/"
    override val priority: Priority = Priority.MULTIPLICATIVE
    override val rightAssoc: Boolean = false
    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = op1 / op2

    override val name2: String = "Quotient"
    override val doc: String = "Dividiert einen Wert durch einen Anderen."
  }

  object modulo extends Operator {
    override val name: String = "%"
    override val priority: Priority = Priority.MULTIPLICATIVE
    override val rightAssoc: Boolean = false
    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = op1 % op2

    override val name2: String = "Modulo"
    override val doc: String = "Berechnet den Divisionsrest der Ganzzahlen-Division mit den beiden Operanden."
  }

  object power extends Operator {
    override val name: String = "^"
    override val priority: Priority = Priority.POWER
    override val rightAssoc: Boolean = true
    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = Math.pow(op1, op2)

    override val name2: String = "Potenz"
    override val doc: String = "Potenziert zwei Werte."
  }

  object equal extends Operator {
    override val name: String = "="
    override val priority: Priority = Priority.COMPARISON
    override val rightAssoc: Boolean = false
    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = if (op1 == op2) 1 else 0

    override val name2: String = "Gleichheit"
    override val doc: String = "Gibt 1 zurück, wenn die Werte gleich sind, sonst 0."
  }

  object lower extends Operator {
    override val name: String = "<"
    override val priority: Priority = Priority.COMPARISON
    override val rightAssoc: Boolean = false
    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = if (op1 < op2) 1 else 0

    override val name2: String = "Kleiner als"
    override val doc: String = "Gibt 1 zurück, wenn der erste Wert kleiner als der zweite ist, sonst 0."
  }

  object greater extends Operator {
    override val name: String = ">"
    override val priority: Priority = Priority.COMPARISON
    override val rightAssoc: Boolean = false
    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = if (op1 > op2) 1 else 0

    override val name2: String = "Größer als"
    override val doc: String = "Gibt 1 zurück, wenn der erste Wert größer als der zweite ist, sonst 0."
  }

  object not_equal extends Operator {
    override val name: String = "≠"
    override val priority: Priority = Priority.COMPARISON
    override val rightAssoc: Boolean = false

    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = AUtil.fromBool(op1 != op2)

    override val name2: String = "Ungleichheit"
    override val doc: String = "Gibt 0 zurück, wenn die Werte gleich sind, sonst 1."
  }

  object lower_equal extends Operator {
    override val name: String = "≤"
    override val priority: Priority = Priority.COMPARISON
    override val rightAssoc: Boolean = false

    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = AUtil.fromBool(op1 <= op2)

    override val name2: String = "Kleiner gleich"
    override val doc: String = "Gibt 1 zurück, wenn der erste Wert kleiner oder gleich dem zweiten ist, sonst 0."
  }

  object greater_equal extends Operator {
    override val name: String = "≥"
    override val priority: Priority = Priority.COMPARISON
    override val rightAssoc: Boolean = false

    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = AUtil.fromBool(op1 >= op2)

    override val name2: String = "Größer gleich"
    override val doc: String = "Gibt 1 zurück, wenn der erste Wert größer oder gleich dem zweiten ist, sonst 0."
  }

  object is_element extends Operator {
    override val name: String = "∈"
    override val priority: Priority = Priority.ELEMENT
    override val rightAssoc: Boolean = false

    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = MDefault.has.result(functionPointers, op2, op1)

    override val name2: String = "Elemnt"
    override val doc: String = "Gibt 1 zurück, wenn der erste Wert im zweiten Wert enthalten ist, sonst 0. Der Zweite Wert muss eine Liste sein."
  }

  object not_element extends Operator {
    override val name: String = "∉"
    override val priority: Priority = Priority.ELEMENT
    override val rightAssoc: Boolean = false

    override def apply(functionPointers: FuncData, op1: Double, op2: Double): Double = AUtil.fromBool(!AUtil.getBool(MDefault.has.result(functionPointers, op2, op1)))

    override val name2: String = "Kein Element"
    override val doc: String = "Gibt 0 zurück, wenn der erste Wert im zweiten Wert enthalten ist, sonst 1. Der Zweite Wert muss eine Liste sein."
  }

  object negate extends Unary {
    override val name: String = "-"
    override def apply(functionPointers: FuncData, op: Double): Double = -op

    override val name2: String = "Negation"
    override val doc: String = "Negiert einen Wert."
  }

  object negateTilde extends Unary {
    override val name: String = "~"
    override def apply(functionPointers: FuncData, op: Double): Double = -op

    override val name2: String = "Negation (alternativ)"
    override val doc: String = "Negiert einen Wert. Da die Tilde nicht zusätzlich als Subtraktions-Operator definiert ist, muss eine negative Zahl nicht so häufig geklammert werden."
  }

  object degreeSign extends PostfixUnary {
    override val name: String = "°"
    override def apply(functionPointers: FuncData, op: Double): Double = Math.toRadians(op)

    override val name2: String = "Grad"
    override val doc: String = "Wandelt eine Zahl vom Gradmaß ins Bogenmaß um. Man kann somit schreiben: <tt>sin(90°)</tt>."
  }

  object factorial extends PostfixUnary {
    override val name: String = "!"
    override def apply(functionPointers: FuncData, op: Double): Double = if (op.isPosInfinity)
      Double.PositiveInfinity
    else Math.round(op) match {
      case x if x < 0 => Double.NaN
      case x if x == 0 => 1
      case x => x.toDouble * apply(functionPointers, (x - 1).toDouble)
    }

    override val name2: String = "Fakultät"
    override val doc: String = "Berechnet die Fakultät einer Zahl. Die Fakultät einer Zahl ist das Produkt der Zahl mit allen kleineren Zahlen."
  }

  object absolute extends MFunction {
    override val name: String = "abs"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.abs(param(0))

    override val name2: String = "Betrag"
    override val doc: String = "Berechnet den Betrag einer Zahl. Der Betrag einer Zahl ist der Abstand dieser Zahl zu 0."
  }

  object degree extends MFunction {
    override val name: String = "deg"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.toDegrees(param(0))

    override val name2: String = "Uwandlung ins Gradmaß"
    override val doc: String = "Wandelt einen Wert vom Bogenmaß ins Gradmaß um."
  }

  object radian extends MFunction {
    override val name: String = "rad"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.toRadians(param(0))

    override val name2: String = "Umwandlung ins Bogenmaß"
    override val doc: String = "Wandelt einen Wert vom Gradmaß ins Bogenmaß um."
  }

  object pi extends Constant {
    override val name: String = "pi"
    override val value: Double = Math.PI

    override val name2: String = "Kreiszahl"
    override val doc: String = "Die Kreiszahl π."
  }

  object e extends Constant {
    override val name: String = "e"
    override val value: Double = Math.E

    override val name2: String = "Eulersche Zahl"
    override val doc: String = "Die eulersche Zahl."
  }

  object inf extends Constant {
    override val name: String = "inf"
    override val value: Double = Double.PositiveInfinity

    override val name2: String = "Unendlich"
    override val doc: String = "Die größte Zahl."
  }

  object sqrt extends MFunction {
    override val name: String = "sqrt"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.sqrt(param(0))

    override val name2: String = "Quadratwurzel"
    override val doc: String = "Berechnet die Quadratwurzel einer Zahl."
  }

  object cbrt extends MFunction {
    override val name: String = "cbrt"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.cbrt(param(0))

    override val name2: String = "Kubikwurzel"
    override val doc: String = "Berechnet die Kubikwurzel einer Zahl."
  }

  object sin extends MFunction {
    override val name: String = "sin"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.sin(param(0))

    override val name2: String = "Sinus"
    override val doc: String = "Berechnet den Sinus eines Wertes im Bogenmaß. Um den Sinus eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object cos extends MFunction {
    override val name: String = "cos"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.cos(param(0))

    override val name2: String = "Kosinus"
    override val doc: String = "Berechnet den Kosinus eines Wertes im Bogenmaß. Um den Kosinus eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object tan extends MFunction {
    override val name: String = "tan"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.tan(param(0))

    override val name2: String = "Tangens"
    override val doc: String = "Berechnet den Tangens eines Wertes im Bogenmaß. Um den Tangens eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object cot extends MFunction {
    override val name: String = "cot"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = 1d / Math.tan(param(0))

    override val name2: String = "Kotangens"
    override val doc: String = "Berechnet den Kotangens eines Wertes im Bogenmaß. Um den Kotangens eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object sec extends MFunction {
    override val name: String = "sec"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = 1d / Math.cos(param(0))

    override val name2: String = "Sekans"
    override val doc: String = "Berechnet den Sekans eines Wertes im Bogenmaß. Um den Sekans eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object csc extends MFunction {
    override val name: String = "csc"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = 1d / Math.sin(param(0))

    override val name2: String = "Kosekans"
    override val doc: String = "Berechnet den Kosekans eines Wertes im Bogenmaß. Um den Kosekans eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object asin extends MFunction {
    override val name: String = "asin"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.asin(param(0))

    override val name2: String = "Arkussinus"
    override val doc: String = "Berechnet den Arkussinus eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden. Der Arkussinus invertiert das erste Sinusintervall."
  }

  object acos extends MFunction {
    override val name: String = "acos"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.acos(param(0))

    override val name2: String = "Arkuskosinus"
    override val doc: String = "Berechnet den Arkuskosinus eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden. Der Arkuskosinus invertiert das erste Kosinusintervall."
  }

  object atan extends MFunction {
    override val name: String = "atan"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.atan(param(0))

    override val name2: String = "Arkustangens"
    override val doc: String = "Berechnet den Arkustangens eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden. Der Arkustangens invertiert das erste Tangensintervall."
  }

  object acot extends MFunction {
    override val name: String = "acot"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.atan(1d / param(0))

    override val name2: String = "Arkuskotangens"
    override val doc: String = "Berechnet den Arkuskotangens eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden. Der Arkuskotangens invertiert das erste Kotangensintervall."
  }

  object asec extends MFunction {
    override val name: String = "asec"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.acos(1d / param(0))

    override val name2: String = "Arkussekans"
    override val doc: String = "Berechnet den Arkussekans eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden. Der Arkuskotangens invertiert das erste Kotangensintervall."
  }

  object acsc extends MFunction {
    override val name: String = "acsc"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.asin(1d / param(0))

    override val name2: String = "Arkuskosekans"
    override val doc: String = "Berechnet den Arkuskosekans eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden. Der Arkuskotangens invertiert das erste Kotangensintervall."
  }

  object atan2 extends MFunction {
    override val name: String = "atan2"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = Math.atan2(param(0), param(1))

    override val name2: String = "Arkustangens"
    override val doc: String = "Berechnet den Arkustangens mit zwei Argumenten. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden."
  }

  object sinh extends MFunction {
    override val name: String = "sinh"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.sinh(param(0))

    override val name2: String = "Hyperbelsinus"
    override val doc: String = "Berechnet den Hyperbelsinus eines Wertes im Bogenmaß. Um den Hyperbelsinus eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object cosh extends MFunction {
    override val name: String = "cosh"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.cosh(param(0))

    override val name2: String = "Hyperbelkosinus"
    override val doc: String = "Berechnet den Hyperbelkosinus eines Wertes im Bogenmaß. Um den Hyperbelkosinus eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object tanh extends MFunction {
    override val name: String = "tanh"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.tanh(param(0))

    override val name2: String = "Hyperbeltangens"
    override val doc: String = "Berechnet den Hyperbeltangens eines Wertes im Bogenmaß. Um den Hyperbeltangens eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object coth extends MFunction {
    override val name: String = "coth"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = 1d / Math.tanh(param(0))

    override val name2: String = "Hyperbelkotangens"
    override val doc: String = "Berechnet den Hyperbelkotangens eines Wertes im Bogenmaß. Um den Hyperbelkotangens eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object sech extends MFunction {
    override val name: String = "sech"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = 1 / Math.cosh(param(0))

    override val name2: String = "Hyperbelsekans"
    override val doc: String = "Berechnet den Hyperbelsekans eines Wertes im Bogenmaß. Um den Hyperbelsekans eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object csch extends MFunction {
    override val name: String = "csch"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = 1 / Math.sinh(param(0))

    override val name2: String = "Hyperbelkosekans"
    override val doc: String = "Berechnet den Hyperbelkosekans eines Wertes im Bogenmaß. Um den Hyperbelkosekans eines Wertes im Gradmaß zu bestimmen, kann ein ° an den Wert angehängt werden."
  }

  object asinh extends MFunction {
    override val name: String = "asinh"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.log(param(0) + Math.sqrt((param(0) * param(0)) + 1.0))

    override val name2: String = "Arkushyperbelsinus"
    override val doc: String = "Berechnet den Arkushyperbelsinus eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden."
  }

  object acosh extends MFunction {
    override val name: String = "acosh"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.log(param(0) + Math.sqrt((param(0) * param(0)) - 1.0))

    override val name2: String = "Arkushyperbelkosinus"
    override val doc: String = "Berechnet den Arkushyperbelkosinus eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden."
  }

  object atanh extends MFunction {
    override val name: String = "atanh"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.log(((1 / param(0)) + 1.0) / ((1 / param(0)) - 1.0)) / 2

    override val name2: String = "Arkushyperbeltangens"
    override val doc: String = "Berechnet den Arkushyperbeltangens eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden."
  }

  object acoth extends MFunction {
    override val name: String = "acoth"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.log((param(0) + 1.0) / (param(0) - 1.0)) / 2

    override val name2: String = "Arkushyperbelkotangens"
    override val doc: String = "Berechnet den Arkushyperbelkotangens eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden."
  }

  object asech extends MFunction {
    override val name: String = "asech"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = acosh.result(functionPointers, 1 / param(0))

    override val name2: String = "Arkushyperbelsekans"
    override val doc: String = "Berechnet den Arkushyperbelsekans eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden."
  }

  object acsch extends MFunction {
    override val name: String = "acsch"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = asinh.result(functionPointers, 1 / param(0))

    override val name2: String = "Arkushyperbelkosekans"
    override val doc: String = "Berechnet den Arkushyperbelkosekans eines Wertes. Das Ergebnis ist im Bogenmaß. Um ein Ergebnis im Gradmaß zu bestimmen, kann die Funktion <tt>deg</tt> verwendet werden."
  }

  object log extends MFunction {
    override val name: String = "log"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = Math.log(param(1)) / Math.log(param(0))

    override val name2: String = "Logarithmus"
    override val doc: String = "<tt>log(n, a)</tt> Berechnet den Logarithmus von <tt>a</tt> zur Basis <tt>n</tt>."
  }

  object ln extends MFunction {
    override val name: String = "ln"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.log(param(0))

    override val name2: String = "Natürlicher Logarithmus"
    override val doc: String = "Berechnet den Logarithmus zur Basis <tt>e</tt> eines Wertes."
  }

  object log10 extends MFunction {
    override val name: String = "log"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.log10(param(0))

    override val name2: String = "Dekadischer Logarithmus"
    override val doc: String = "Berechnet den Logarithmus zur Basis 10 eines Wertes."
  }

  object root extends MFunction {
    override val name: String = "root"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = Math.pow(param(1), 1 / param(0))

    override val name2: String = "Wurzel"
    override val doc: String = "<tt>root(n, a)</tt> Berechnet die <tt>n</tt>-te Wurzel von <tt>a</tt>."
  }

  object max extends MFunction {
    override val name: String = "max"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = param.fold(Double.NegativeInfinity)((d1, d2) => Math.max(d1, d2))

    override val name2: String = "Maximierung"
    override val doc: String = "Gibt das Größte aller Argumente Zurück."
  }

  object min extends MFunction {
    override val name: String = "min"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = param.fold(Double.PositiveInfinity)((d1, d2) => Math.min(d1, d2))

    override val name2: String = "Minimierung"
    override val doc: String = "Gibt das kleinste aller Argumente zurück."
  }

  object mean extends MFunction {
    override val name: String = "mean"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = (param.map(d => BigDecimal(d, MathContext.DECIMAL128)).sum / BigDecimal(param.size, MathContext.DECIMAL128)).toDouble

    override val name2: String = "Arithmetisches Mittel"
    override val doc: String = "Berechnet das arithmetische Mittel aller Argumente."
  }

  object gmean extends MFunction {
    override val name: String = "gmean"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = Math.pow(param.product, 1.0 / param.size.toDouble)


    override val name2: String = "Geometrisches Mittel"
    override val doc: String = "Berechnet das geometrische Mittel aller Argumente."
  }

  object hmean extends MFunction {
    override val name: String = "hmean"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = if (param.contains(0)) { 0 } else { (BigDecimal(param.size, MathContext.DECIMAL128) / param.map(d => BigDecimal(1, MathContext.DECIMAL128) / BigDecimal(d, MathContext.DECIMAL128)).sum).toDouble }

    override val name2: String = "Harmonisches Mittel"
    override val doc: String = "Berechnet das harmonische Mittel aller Argumente."
  }

  object clamp extends MFunction {
    override val name: String = "clamp"
    override val params: Int = 3
    override def result(functionPointers: FuncData, param: Double*): Double = {
      if (param(1) > param(2)) {
        Math.min(param(1), Math.max(param(2), param(0)))
      } else {
        Math.min(param(2), Math.max(param(1), param(0)))
      }
    }

    override val name2: String = "Einschränken"
    override val doc: String = "Gibt das erste Argument zurück, wenn es zwischen den beiden letzten Argumenten liegt. Sonst wird das Argument, das näher am ersten Argument zurückgegeben (das letze oder das vorletzte Argument). Beispiele: <tt>clamp(15, 10, 20) = 15   clamp(5, 10, 20) = 10   clamp(25, 10, 20) = 20</tt>."
  }

  object round extends MFunction {
    override val name: String = "round"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0).isInfinite || param(0).isNaN) param(0) else Math.round(param(0)).toDouble

    override val name2: String = "Einfaches Runden"
    override val doc: String = "Rundet den gegebenen Wert auf eine ganze Zahl."
  }

  object round2 extends MFunction {
    override val name: String = "round"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0).isInfinite || param(0).isNaN || param(1).isInfinite || param(1).isNaN) param(0) else BigDecimal(param(0)).setScale(param(1).toInt, RoundingMode.HALF_UP).toDouble

    override val name2: String = "Runden"
    override val doc: String = "<tt>round(a, b)</tt> rundet <tt>a</tt> auf <tt>b</tt> Nachkommastellen. Negative Werte für <tt>b</tt> sind erlaubt."
  }

  object floor extends MFunction {
    override val name: String = "floor"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.floor(param(0))

    override val name2: String = "Einfaches Abrunden"
    override val doc: String = "Rundet einen Wert ab."
  }

  object floor2 extends MFunction {
    override val name: String = "floor"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0).isInfinite || param(0).isNaN || param(1).isInfinite || param(1).isNaN) param(0) else BigDecimal(param(0)).setScale(param(1).toInt, RoundingMode.FLOOR).toDouble

    override val name2: String = "Abrunden"
    override val doc: String = "<tt>floor(a, b)</tt> rundet <tt>a</tt> auf <tt>b</tt> Nachkommastellen ab. Negative Werte für <tt>b</tt> sind erlaubt."
  }

  object ceil extends MFunction {
    override val name: String = "ceil"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = Math.ceil(param(0))

    override val name2: String = "Einfaches Aufrunden"
    override val doc: String = "Rundet einen Wert auf."
  }

  object ceil2 extends MFunction {
    override val name: String = "ceil"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0).isInfinite || param(0).isNaN || param(1).isInfinite || param(1).isNaN) param(0) else BigDecimal(param(0)).setScale(param(1).toInt, RoundingMode.CEILING).toDouble

    override val name2: String = "Aufrunden"
    override val doc: String = "<tt>floor(a, b)</tt> rundet <tt>a</tt> auf <tt>b</tt> Nachkommastellen auf. Negative Werte für <tt>b</tt> sind erlaubt."
  }

  object rand0 extends MFunction {
    override val name: String = "rand"
    override val params: Int = 0
    override def result(functionPointers: FuncData, param: Double*): Double = Math.random()

    override val name2: String = "Einfacher Zufall"
    override val doc: String = "Gibt einen Zufallswert zwischen 0 (inklusiv) und 1 (exklusiv) zurück."
  }

  object rand extends MFunction {
    override val name: String = "rand"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0) == 0) 0 else if (param(0) < 0) new Random().between(param(0), 0) else new Random().between(0, param(0))

    override val name2: String = "Einfacher Zufall"
    override val doc: String = "Gibt einen Zufallswert zwischen 0 (inklusiv) und dem gegebenen Wert (exklusiv) zurück."
  }

  object rand2 extends MFunction {
    override val name: String = "rand"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0) == param(1)) param(0) else if (param(0) < param(1)) new Random().between(param(0), param(1)) else new Random().between(param(1), param(0))

    override val name2: String = "Zufall"
    override val doc: String = "Gibt einen Zufallswert zwischen dem kleineren Wert (inklusiv) und dem größeren Wert (exklusiv) zurück."
  }

  object randint extends MFunction {
    override val name: String = "randint"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0) < param(1)) param(0) + new Random().nextInt(param(1).toInt - param(0).toInt + 1) else param(1) + new Random().nextInt(param(0).toInt - param(1).toInt + 1)

    override val name2: String = "Ganzzahlen-Zufall"
    override val doc: String = "Gibt einen Zufallswert zwischen beiden Werten (inklusiv) zurück."
  }

  object randchoice extends MFunction {
    override val name: String = "randchoice"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = param(new Random().nextInt(param.size))

    override val name2: String = "Zufallsauswahl"
    override val doc: String = "Gibt einen der angegebenen Argumente zurück."
  }

  object knuth extends MFunction {
    override val name: String = "knuth"
    override val params: Int = 3
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0).isPosInfinity || param(2).isPosInfinity)
      Double.PositiveInfinity
    else if (param(2) <= 0 && Math.round(param(0)) >= 2)
      Double.NaN
    else Math.round(param(0)) match {
      case x if x <= -3 => Double.NaN
      case -2 => param(2) + 1
      case -1 => param(1) + param(2)
      case 0 => param(1) * param(2)
      case 1 => Math.pow(param(1), param(2))
      case x =>
        if (param(2) == 1)
          result(functionPointers, (x - 1).toDouble, param(1), param(1))
        var r = param(1)
        for (_ <- 1 until param(2).toInt) {
          if (r.isPosInfinity)
            return Double.PositiveInfinity
          else
            r = result(functionPointers, (x - 1).toDouble, param(1), r)
        }
        r
    }

    override val name2: String = "Knuths Pfeilnotation"
    override val doc: String = "<tt>knuth(n, a, b) = a ￪<sup>n</sup> b</tt> Siehe Knuths Pfeilnotation auf Wikipedia."
  }

  object rknuth extends MFunction {
    override val name: String = "rknuth"
    override val params: Int = 3
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0).isPosInfinity || param(2).isPosInfinity)
      Double.PositiveInfinity
    else if (param(2) <= 0 && Math.round(param(0)) >= 2)
      Double.NaN
    else Math.round(param(0)) match {
      case x if x <= -3 => Double.NaN
      case -2 => param(2) - 1
      case -1 => param(1) - param(2)
      case 0 => param(1) / param(2)
      case 1 => Math.log(param(2)) / Math.log(param(1))
      case x =>
        var r = 1
        var v = param(2)
        while(v > param(1)) {
          v = result(functionPointers, (x - 1).toDouble, param(1), v)
          if (v > param(2))
            return Double.PositiveInfinity
          r += 1
        }
        r
    }

    override val name2: String = "Umgekehrte Pfeilnotation"
    override val doc: String = "<tt>knuth(n, a, b) = a ￬<sup>n</sup> b</tt> Siehe https://www.johndcook.com/blog/2018/04/10/up-arrow-and-down-arrow-notation/."
  }

  object sum extends MFunction {
    override val name: String = "sum"
    override val params: Int = 3
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(2).toInt)
      if (mf == null || (mf.params != 1 && mf.params != Parser.VARARG)) { // Vararg check
        Double.NaN
      } else {
        var sum: Double = 0
        val range = if (param(0) <= param(1)) param(0).toLong to param(1).toLong else (param(1).toLong to param(0).toLong).reverse
        for (i <- range) {
          sum += mf.result(functionPointers, i.toDouble)
        }
        sum
      }
    }

    override val name2: String = "Summenfunktion"
    override val doc: String = "<tt>sum(a, b, f)</tt> ruft den Funktionspointer <tt>f</tt> für jeden Wert von <tt>a</tt> bis <tt>b</tt> (beides inklusiv) auf und addiert die Ergebnisse."
  }

  object prod extends MFunction {
    override val name: String = "prod"
    override val params: Int = 3
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(2).toInt)
      if (mf == null || (mf.params != 1 && mf.params != Parser.VARARG)) { // Vararg check
        Double.NaN
      } else {
        var prod: Double = 1
        val range = if (param(0) <= param(1)) param(0).toLong to param(1).toLong else (param(1).toLong to param(0).toLong).reverse
        for (i <- range) {
          prod *= mf.result(functionPointers, i.toDouble)
        }
        prod
      }
    }

    override val name2: String = "Produktfunktion"
    override val doc: String = "<tt>prod(a, b, f)</tt> ruft den Funktionspointer <tt>f</tt> für jeden Wert von <tt>a</tt> bis <tt>b</tt> (beides inklusiv) auf und multipliziert die Ergebnisse."
  }

  object foldl extends MFunction {
    override val name: String = "foldl"
    override val params: Int = 4
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(2).toInt)
      val folder = functionPointers.get(param(3).toInt)
      if (mf == null || (mf.params != 1 && mf.params != Parser.VARARG)) { // Vararg check
        Double.NaN
      } else if (folder == null || (folder.params != 2 && folder.params != Parser.VARARG)) {
        Double.NaN
      } else {
        var sum: Double = Double.NaN
        var isFirst = true
        val range = if (param(0) <= param(1)) param(0).toLong to param(1).toLong else (param(1).toLong to param(0).toLong).reverse
        for (i <- range) {
          if (isFirst) {
            sum = mf.result(functionPointers, i.toDouble)
            isFirst = false
          } else {
            sum = folder.result(functionPointers, sum, mf.result(functionPointers, i.toDouble))
          }
        }
        sum
      }
    }

    override val name2: String = "Faltfunktion"
    override val doc: String = "<tt>fold(a, b, f, g)</tt> ruft den Funktionspointer <tt>f</tt> für jeden Wert von <tt>a</tt> bis <tt>b</tt> (beides inklusiv) auf und nutzt den Funktionpointer <tt>g</tt> (2 Argumente) zum zusammenführen. <tt>g</tt> bekommt als erstes Argument den bisher zusammengeführten Wert und als zweites Argument den neu berechneten Wert."
  }

  object foldr extends MFunction {
    override val name: String = "foldr"
    override val params: Int = 4
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(2).toInt)
      val folder = functionPointers.get(param(3).toInt)
      if (mf == null || (mf.params != 1 && mf.params != Parser.VARARG)) { // Vararg check
        Double.NaN
      } else if (folder == null || (folder.params != 2 && folder.params != Parser.VARARG)) {
        Double.NaN
      } else {
        var sum: Double = Double.NaN
        var isFirst = true
        val range = if (param(0) <= param(1)) (param(0).toLong to param(1).toLong).reverse else param(1).toLong to param(0).toLong
        for (i <- range) {
          if (isFirst) {
            sum = mf.result(functionPointers, i.toDouble)
            isFirst = false
          } else {
            sum = folder.result(functionPointers, mf.result(functionPointers, i.toDouble), sum)
          }
        }
        sum
      }
    }

    override val name2: String = "Umgekehrte Faltfunktion"
    override val doc: String = "<tt>fold(a, b, f, g)</tt> ruft den Funktionspointer <tt>f</tt> für jeden Wert von <tt>a</tt> bis <tt>b</tt> (beides inklusiv) auf und nutzt den Funktionpointer <tt>g</tt> (2 Argumente) zum zusammenführen in umgekehrter Reihenfolge. <tt>g</tt> bekommt als zweites Argument den bisher zusammengeführten Wert und als erstes Argument den neu berechneten Wert."
  }

  object foldv extends MFunction {
    override val name: String = "foldv"
    override val params: Int = 4
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(2).toInt)
      val folder = functionPointers.get(param(3).toInt)
      if (mf == null || (mf.params != 1 && mf.params != Parser.VARARG)) { // Vararg check
        Double.NaN
      } else if (folder == null) {
        Double.NaN
      } else {
        val lb = ListBuffer[Double]()
        val range = if (param(0) <= param(1)) param(0).toLong to param(1).toLong else (param(1).toLong to param(0).toLong).reverse
        for (i <- range) {
          lb += mf.result(functionPointers, i.toDouble)
        }
        if (folder.params != lb.length && folder.params != Parser.VARARG) {
          Double.NaN
        } else {
          folder.result(functionPointers, lb.toSeq: _*)
        }
      }
    }

    override val name2: String = "Vararg-Faltfunktion"
    override val doc: String = "<tt>fold(a, b, f, g)</tt> ruft den Funktionspointer <tt>f</tt> für jeden Wert von <tt>a</tt> bis <tt>b</tt> (beides inklusiv) auf und nutzt den Funktionpointer <tt>g</tt> zum zusammenführen. <tt>g</tt> bekommt <b>alle</b> berechneten Werte als Argumente."
  }

  object foldla extends MFunction {
    override val name: String = "foldla"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(0).toInt)
      val folder = functionPointers.get(param(1).toInt)
      if (mf == null || (mf.params != 1 && mf.params != Parser.VARARG)) { // Vararg check
        Double.NaN
      } else if (folder == null || (folder.params != 2 && folder.params != Parser.VARARG)) {
        Double.NaN
      } else if (param.size <= 2) {
        Double.NaN
      } else {
        var sum: Double = Double.NaN
        var isFirst = true
        for (i <- param.slice(2, param.size)) {
          if (isFirst) {
            sum = mf.result(functionPointers, i)
            isFirst = false
          } else {
            sum = folder.result(functionPointers, sum, mf.result(functionPointers, i))
          }
        }
        sum
      }
    }

    override val name2: String = "Listen-Faltfunktion"
    override val doc: String = "<tt>folda(f, g, ...)</tt> ruft den Funktionspointer <tt>f</tt> für jeden Wert der übrigen Argumente (<tt>...</tt>) auf und nutzt den Funktionspointer <tt>g</tt> zum zusammenführen der Ergebnisse. Siehe dazu <tt>fold</tt>."
  }

  object foldra extends MFunction {
    override val name: String = "foldra"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(0).toInt)
      val folder = functionPointers.get(param(1).toInt)
      if (mf == null || (mf.params != 1 && mf.params != Parser.VARARG)) { // Vararg check
        Double.NaN
      } else if (folder == null || (folder.params != 2 && folder.params != Parser.VARARG)) {
        Double.NaN
      } else if (param.size <= 2) {
        Double.NaN
      } else {
        var sum: Double = Double.NaN
        var isFirst = true
        for (i <- param.slice(2, param.size).reverse) {
          if (isFirst) {
            sum = mf.result(functionPointers, i)
            isFirst = false
          } else {
            sum = folder.result(functionPointers, mf.result(functionPointers, i), sum)
          }
        }
        sum
      }
    }

    override val name2: String = "Umgekehrte Listen-Faltfunktion"
    override val doc: String = "<tt>folda(f, g, ...)</tt> ruft den Funktionspointer <tt>f</tt> für jeden Wert der übrigen Argumente (<tt>...</tt>) auf und nutzt den Funktionspointer <tt>g</tt> zum zusammenführen der Ergebnisse in umgekehrter Reihenfolge. Siehe dazu <tt>fold</tt>."
  }

  object foldva extends MFunction {
    override val name: String = "foldva"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(0).toInt)
      val folder = functionPointers.get(param(1).toInt)
      if (mf == null || (mf.params != 1 && mf.params != Parser.VARARG)) { // Vararg check
        Double.NaN
      } else if (folder == null) {
        Double.NaN
      } else if (param.size <= 2) {
        Double.NaN
      } else {
        val lb = ListBuffer[Double]()
        for (i <- param.slice(2, param.size)) {
          lb += mf.result(functionPointers, i)
        }
        if (folder.params != lb.length && folder.params != Parser.VARARG) {
          Double.NaN
        } else {
          folder.result(functionPointers, lb.toSeq: _*)
        }
      }
    }

    override val name2: String = "Listen-Vararg-Faltfunktion"
    override val doc: String = "<tt>foldva(f, g, ...)</tt> ruft den Funktionspointer <tt>f</tt> für jeden Wert der übrigen Argumente (<tt>...</tt>) auf und nutzt den Funktionspointer <tt>g</tt> zum zusammenführen der Ergebnisse in einem Aufruf. Siehe dazu <tt>foldv</tt>."
  }

  object fmap extends MFunction {
    override val name: String = "fmap"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf1 = functionPointers.get(param(0).toInt)
      val mf2 = functionPointers.get(param(1).toInt)
      if (mf2.params != 1 && mf2.params != Parser.VARARG) {
        -1
      } else {
        functionPointers.add(new MFunction {
          override val name: String = ""
          override val params: Int = mf1.params
          override def result(newFP: FuncData, param: Double*): Double = {
            mf2.result(newFP, mf1.result(newFP, param: _*))
          }
        })
      }
    }

    override val name2: String = "Funktionsmapping"
    override val doc: String = "Erwartet zwei Funktionspointer. Gibt einen neuen Funktionspointer zurück, der zuerst die erste Funktion aufruft und danach die zweite Funktion mit dem Ergebnis der ersten Funktion."
  }

  object call extends MFunction {
    override val name: String = "call"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(0).toInt)
      val args = param.size - 1
      if (AUtil.correctParams(mf, args)) {
        mf.result(functionPointers, param.slice(1, param.size): _*)
      } else {
        Double.NaN
      }
    }

    override val name2: String = "Funktionsaufruf"
    override val doc: String = "Erwartet als erstes Argument einen Funktionspointer. Ruft diesen mit den restlichen Argumenten auf."
  }

  object fc extends MFunction {
    override val name: String = "fc"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      var default: MFunction = null
      val funcs = ListBuffer[(MFunction, MFunction)]()
      var params: Option[Int] = None
      for (i <- 0.until(param.size, 2)) {
        val func = functionPointers.get(param(i).toInt)
        if (func == null) {
          return -1
        }
        params = AUtil.merge(params, func.params)
        if (params.isEmpty) {return -1}
        if ((i + 1) < param.size) {
          val func2 = functionPointers.get(param(i + 1).toInt)
          if (func2 == null) {
            return -1
          }
          params = AUtil.merge(params, func2.params)
          if (params.isEmpty) {return -1}
          funcs.addOne((func, func2))
        } else {
          default = func
        }
      }

      if (params.isEmpty) {
        return -1
      }
      val functionParams = params.get

      functionPointers.add(new MFunction {
        override val name: String = ""
        override val params: Int = functionParams

        override def result(functionPointers: FuncData, param: Double*): Double = {
          for ((predicate, func) <- funcs) {
            if (AUtil.getBool(predicate.result(functionPointers, param: _*))) {
              return func.result(functionPointers, param: _*)
            }
          }
          if (default != null) {
            default.result(functionPointers, param: _*)
          } else {
            Double.NaN
          }
        }
      })
    }

    override val name2: String = "Function Comprehension"
    override val doc: String = "Setzt eine Funktion aus mehreren Teilfunktionen zusammen. Erwartet zuerst eine beliebige Anzahl an Paaren von Filter-Funktionen und Ergebnis-Funktionen. Die Filter-Funktionen werden in der angegebenen Reihe aufgerufen. Bei der ersten, die <tt>true</tt> zurück gibt, wird die passende Ergebnis-Funktion aufgerufen, um ein Ergebnis zu erhalten. Ist die Anzahl der Parameter ungerade, wird, falls keine Filter-Funktion gefunden wurde die letzte gegebene Funktion aufgerufen, sonst wird NaN zurück gegeben. Beispiel: <tt>fc(@[x > 0], @[x], @[-x]</tt> entspricht <tt>@abs</tt>"
  }

  object lcm extends MFunction {
    override val name: String = "lcm"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      if (param.size <= 0) {
        Double.NaN
      } else if (param.size == 1) {
        Math.floor(param(0))
      } else {
        var lcm: BigInt = null
        var isFirst = true
        for (v <- param) {
          if (isFirst) {
            lcm = BigInt(v.toLong)
            isFirst = false
          } else {
            val bigv = BigInt(v.toLong)
            lcm = (lcm * bigv).abs / lcm.gcd(bigv).abs
          }
        }
        lcm.toDouble
      }
    }

    override val name2: String = "Kleinstes gemeinsames Vielfaches"
    override val doc: String = "Gibt das kleinste Gemeinsame Vielfache aller Argumente zurück."
  }

  object gcd extends MFunction {
    override val name: String = "gcd"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      if (param.size <= 0) {
        Double.NaN
      } else if (param.size == 1) {
        Math.floor(param(0))
      } else {
        var gcd: BigInt = null
        var isFirst = true
        for (v <- param) {
          if (isFirst) {
            gcd = BigInt(v.toLong)
            isFirst = false
          } else {
            gcd = gcd.gcd(BigInt(v.toLong))
          }
        }
        gcd.toDouble
      }
    }

    override val name2: String = "Größter gemeinsamer Teiler"
    override val doc: String = "Gibt den größten gemeinsamen Teiler aller Argumente zurück."
  }

  object hypot extends MFunction {
    override val name: String = "hypot"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = Math.hypot(param(0), param(1))

    override val name2: String = "Hypotenuse"
    override val doc: String = "Berechnet die Hypotenuse in einem rechtwinkligen Dreieck aus beiden Katheten. <tt>hypot(a, b) = sqrt(a² + b²)</tt>."
  }

  object fparams extends MFunction {
    override val name: String = "fparams"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(0).toInt)
      if (mf == null) {
        Double.NaN
      } else {
        mf.params
      }
    }

    override val name2: String = "Parameter-Anzahl"
    override val doc: String = "Gibt die anzahl der erwarteten Paameter des angegebenen Funktionspointers zurück. <tt>-1</tt> für Vararg-Funktionen."
  }

  object integ extends MFunction {
    private val mc = new MathContext(64, java.math.RoundingMode.HALF_EVEN)

    override val name: String = "integ"
    override val params: Int = 4
    override def result(functionPointers: FuncData, param: Double*): Double = {
      if (param(3) < 0)
        return Double.NaN
      val mf = functionPointers.get(param(2).toInt)
      if (mf == null || (mf.params != 1 && mf.params != Parser.VARARG))
        return Double.NaN
      val start = if (param(0) <= param(1)) param(0) else param(1)
      val end = if (param(0) <= param(1)) param(1) else param(0)
      val span = Math.abs(end - start)
      if (span == 0)
        return 0
      val stepSize = if (param(3) == 0) {
        BigDecimal(span)
      } else {
        BigDecimal(span, mc) / BigDecimal(Math.pow(Math.E, param(3)), mc)  //Math.pow(Math.E, param(3))
      }
      var sum = BigDecimal(0, mc)
      var elems: Long = 0
      var now = BigDecimal(start, mc)
      while (now <= end + Math.ulp(stepSize.toDouble)) {
        elems += 1
        val nextResult = mf.result(functionPointers, now.toDouble)
        if (!nextResult.isNaN && !nextResult.isInfinite) {
          sum += nextResult
        }
        now += stepSize
      }
      val mean = sum / elems
      (mean * span).toDouble
    }

    override val name2: String = "Integral"
    override val doc: String = "<tt>integ(a, b, f, p)</tt> berechnet annäherungsweise ∫<sub>a</sub><sup>b</sup>(f)dx. Dazu wird die Funktion an verschiedenen Stellen aufgerufen. Die Anzahl der Aufrufe entspricht e^p. <tt>p</tt> ist also die Genauigkeit. Negative Genauigkeiten sind nicht erlaubt. Eine Genauigkeit von 0 prüft die Funktion nur am Anfang und am Ende. (Nur für lineare Funktionen zu empfehelen.)"
  }

  object and extends MFunction {
    override val name: String = "and"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = AUtil.fromBool(param.forall(d => AUtil.getBool(d)))

    override val name2: String = "Logisches UND"
    override val doc: String = "Gibt 1 zurück, wenn alle Werte größer als 0 (wahr) sind, sonst 0."
  }

  object or extends MFunction {
    override val name: String = "or"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = AUtil.fromBool(param.exists(d => AUtil.getBool(d)))

    override val name2: String = "Logisches ODER"
    override val doc: String = "Gibt 1 zurück, wenn mindestens ein Wert größer als 0 (wahr) ist, sonst 0."
  }

  object nand extends MFunction {
    override val name: String = "nand"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = AUtil.fromBool(!param.forall(d => AUtil.getBool(d)))

    override val name2: String = "Logisches NICHT-UND"
    override val doc: String = "Gibt 0 zurück, wenn alle Werte größer als 0 (wahr) sind, sonst 1."
  }

  object nor extends MFunction {
    override val name: String = "nor"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = AUtil.fromBool(!param.exists(d => AUtil.getBool(d)))

    override val name2: String = "Logisches NICHT-ODER"
    override val doc: String = "Gibt 0 zurück, wenn mindestens ein Wert größer als 0 (wahr) ist, sonst 1."
  }

  object xor extends MFunction {
    override val name: String = "xor"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = AUtil.fromBool(param.count(d => AUtil.getBool(d)) == 1)

    override val name2: String = "Logisches EXKLUSIV-ODER"
    override val doc: String = "Gibt 1 zurück, wenn genau ein Wert größer als 0 (wahr) ist, sonst 0."
  }

  object not extends MFunction {
    override val name: String = "not"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = AUtil.fromBool(!AUtil.getBool(param(0)))

    override val name2: String = "Logisches NICHT"
    override val doc: String = "Gibt 1 zurück, wenn der Wert nicht größer als 0 (wahr) ist, sonst 0."
  }

  object implic extends MFunction {
    override val name: String = "implic"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = AUtil.fromBool((!AUtil.getBool(param(0))) || AUtil.getBool(param(1)))

    override val name2: String = "Logische IMPLIKATION"
    override val doc: String = "Gibt 0 zurück, wenn der erste Wert nicht größer als 0 ist (falsch) und der zweite Wert größer als 0 ist (wahr), sonst 1"
  }

  object nimplic extends MFunction {
    override val name: String = "nimplic"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = AUtil.fromBool(AUtil.getBool(param(0)) && (!AUtil.getBool(param(1))))

    override val name2: String = "Logische NICHT-IMPLIKATION"
    override val doc: String = "Gibt 1 zurück, wenn der erste Wert nicht größer als 0 ist (falsch) und der zweite Wert größer als 0 ist (wahr), sonst 0"
  }

  object int extends MFunction {
    override val name: String = "int"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = if (param(0).isNaN || param(0).isInfinite) {
      AUtil.fromBool(false)
    } else {
      AUtil.fromBool(BigDecimal(param(0)).isWhole)
    }

    override val name2: String = "Integertest"
    override val doc: String = "Gibt 1 zurück, wenn der gegebene Wert eine ganze Zahl ist. Sonst 0."
  }

  object dice extends MFunction {
    override val name: String = "dice"
    override val params: Int = 0
    override def result(functionPointers: FuncData, param: Double*): Double = new Random().nextInt(6) + 1

    override val name2: String = "Würfel"
    override val doc: String = "Gibt eine zufällige Zahl zwischen 1 und 6 zurück."
  }

  object prime extends MFunction {
    override val name: String = "prime"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = {
      if (param(0).isNaN || param(0).isInfinite)
        return AUtil.fromBool(false)
      val bd = BigDecimal(param(0))
      if (bd <= 1 || !(bd - 1).isValidInt)
        return AUtil.fromBool(false)
      val bi = bd.toBigInt
      if (!bi.isProbablePrime(10))
        return AUtil.fromBool(false)
      val target = bi.toInt
      val bools = ListBuffer.fill(target + 1)(true)
      bools(0) = true
      bools(1) = true
      val limit = Math.ceil(Math.sqrt(target)).toInt
      for (i <- 2 to limit) {
        if (bools(i) && i <= limit) {
          var factor = 2
          while (i * factor <= target) {
            bools(factor * i) = false
            factor += 1
          }
        }
      }
      AUtil.fromBool(bools(target))
    }

    override val name2: String = "Primzahlentest"
    override val doc: String = "Gibt 1 (true) zurück, wenn die gegebene Zahl eine Primzahl ist, sonst 0 (false). Wenn die Zahl zu groß ist, wird NaN zurückgegeben."
  }

  object probablePrime extends MFunction {
    override val name: String = "probablePrime"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = {
      if (param(0).isNaN || param(0).isInfinite)
        return AUtil.fromBool(false)
      val bd = BigDecimal(param(0))
      if (bd <= 1 || !(bd - 1).isValidInt)
        return AUtil.fromBool(false)
      val bi = bd.toBigInt
      AUtil.fromBool(bi.isProbablePrime(param(1).toInt))
    }

    override val name2: String = "Unvollständiger Primzahlentest"
    override val doc: String = "Gibt 1 (true) zurück, wenn die gegebene Zahl eine Primzahl ist, sonst 0 (false). Die Funktion erwartet ein weiteres Argument, dass die Genaugkeit bestimmt. Ein Ergebnis von false ist immer korrekt. Ein ergebnis von true ist mit höherer Wahrscheinlichkeit korrekt, wenn das zweite Argument eine höhere Zahl ist. Die Wahrscheinlichkeit beträgt 1-0.5^b wobei b das zweite Argument ist."
  }

  object nPr extends MFunction {
    override val name: String = "nPr"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = factorial.apply(functionPointers, param(0)) / factorial.apply(functionPointers, param(0) - param(1))

    override val name2: String = "Permutationen"
    override val doc: String = "<tt>nPr(k,n) =</tt> Die Anzahl aller Permutationen bei Auswahl von <tt>k</tt> Objekten aus Insgesamt <tt>n</tt> Objekten."
  }

  object nCr extends MFunction {
    override val name: String = "nCr"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = nPr.result(functionPointers, param(0), param(1)) / factorial.apply(functionPointers, param(1))

    override val name2: String = "Kombinationen"
    override val doc: String = "<tt>nCr(k,n) =</tt> Die Anzahl aller Möglichkeiten bei Auswahl von <tt>k</tt> Objekten aus Insgesamt <tt>n</tt> Objekten ohne Beachtung der Reihenfolge."
  }

  object pit extends MFunction {
    override val name: String = "pit"
    override val params: Int = 8
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val points = ListBuffer[(Double, Double)]()
      var i = 2
      while (i < param.size - 1) {
        points += ((param(i), param(i + 1)))
        i += 2
      }

      val x = param(0)
      val y = param(1)

      println(x)
      println(y)
      println(points)

      //noinspection ZeroIndexToHead
      if (points.size < 3) {
        Double.NaN
      } else if (points.size == 3) {
        val pp0 = (points(2)._1 - points(0)._1, points(2)._2 - points(0)._2)
        val pp1 = (points(1)._1 - points(0)._1, points(1)._2 - points(0)._2)
        val pp2 = (x - points(0)._1, y - points(0)._2)

        val dot00 = (pp0._1 * pp0._1) + (pp0._2 * pp0._2)
        val dot01 = (pp0._1 * pp1._1) + (pp0._2 * pp1._2)
        val dot02 = (pp0._1 * pp2._1) + (pp0._2 * pp2._2)
        val dot11 = (pp1._1 * pp1._1) + (pp1._2 * pp1._2)
        val dot12 = (pp1._1 * pp2._1) + (pp1._2 * pp2._2)

        val invDenom = 1 / ((dot00 * dot11) - (dot01 * dot01))
        val u = ((dot11 * dot02) - (dot01 * dot12)) * invDenom
        val v = ((dot00 * dot12) - (dot01 * dot02)) * invDenom

        AUtil.fromBool((u >= 0) && (v >= 0) && (u + v <= 1))
      } else {
        Double.NaN
      }
    }

    override val name2: String = "Punkt in Dreieck"
    override val doc: String = "<tt>pip(x, y, x_1, y_1, x_2, y_2, x_3, y_3)</tt> Gibt 1 zurück wenn der Punkt (x | y) im Dreieck definiert aus den anderen punkten liegt."
  }


  object list0 extends MFunction {
    override val name: String = "list"
    override val params: Int = 0
    override def result(functionPointers: FuncData, param: Double*): Double = {
      functionPointers.get(FuncData.EMPTY_LIST)
    }

    override val name2: String = "Leere Liste"
    override val doc: String = "Gibt einen Pointer auf eine leere Liste zurück."
  }

  object list extends MFunction {
    override val name: String = "list"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      functionPointers.add(new MFunctionList(param.toList))
    }

    override val name2: String = "Listen-Funktion"
    override val doc: String = "Erstellt eine neue Liste aus allen gegebenen Argumenten und gibt einen Pointer zurück."
  }
  
  object vec extends MFunction {
    override val name: String = "vec"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      functionPointers.add(new MFunctionList(param.toList))
    }

    override val name2: String = "Vektor"
    override val doc: String = "Erstellt eine neue Liste aus allen gegebenen Argumenten und gibt einen Pointer zurück. Alternativer Name für <tt>list</tt>"
  }

  object lmap extends MFunction {
    override val name: String = "lmap"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val mf = functionPointers.get(param(1).toInt)
      val listFunction = functionPointers.get(param(0).toInt)
      if (mf == null || listFunction == null || !AUtil.correctParams(mf, 1) || !AUtil.correctParams(listFunction, 1)) {
        Double.NaN
      } else {
        val length = AUtil.getListLength(functionPointers, listFunction)
        if (length < 0) {
          return Double.NaN
        }
        functionPointers.add(new MFunctionLazyList(length, idx => mf.result(functionPointers, listFunction.result(functionPointers, idx))))
      }
    }

    override val name2: String = "Listen-Mapping"
    override val doc: String = "Erwartet als erstes Argument eine Liste und als zweites Argument eine Funktion mit einem Argument. Erzeugt eine neue Liste, für die jedes Element der alten Liste mit der gegebenen Funktion aufgerufen wurde."
  }

  object len extends MFunction {
    override val name: String = "len"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val listFunction = functionPointers.get(param(0).toInt)
      if (listFunction == null || !AUtil.correctParams(listFunction, 1)) {
        Double.NaN
      } else {
        val length = AUtil.getListLength(functionPointers, listFunction)
        if (length < 0) {
          Double.NaN
        } else {
          length
        }
      }
    }

    override val name2: String = "Listen-Länge"
    override val doc: String = "Erwartet als Argument eine Liste und gibt ihre Länge zurück."
  }

  object range extends MFunction {
    override val name: String = "range"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val start = param(0).toInt
      val end = param(1).toInt
      if (start == end) {
        functionPointers.get(FuncData.EMPTY_LIST)
      } else if (start < end) {
        functionPointers.add(new MFunctionList((start to end).map(_.toDouble)))
      } else {
        functionPointers.add(new MFunctionList((end to start).map(_.toDouble).reverse))
      }
    }

    override val name2: String = "Reihe"
    override val doc: String = "<tt>range(a, b)</tt> gibt eine Liste mit allen Werten von a bis b zurück (beides inklusiv, ganze Zahlen"
  }

  object range2 extends MFunction {
    override val name: String = "range"
    override val params: Int = 3
    override def result(functionPointers: FuncData, param: Double*): Double = {
      var start = param(0)
      var end = param(1)
      val run = Math.abs(param(2))
      if (run == 0) {
        return Double.NaN
      }
      val reverse = start > end != param(2) < 0
      if (start == end) {
        functionPointers.get(FuncData.EMPTY_LIST)
      } else if (start > end) {
        val tmp = start
        start = end
        end = tmp
      }
      val lb = ListBuffer[Double]()
      var current = start
      while (current <= end) {
        lb += current
        current += run
      }
      if (reverse) {
        functionPointers.add(new MFunctionList(lb.reverse.toList))
      } else {
        functionPointers.add(new MFunctionList(lb.toList))
      }
    }

    override val name2: String = "Reihe mit Schrittweite"
    override val doc: String = "<tt>range(a, b, n)</tt> gibt eine Liste mit allen Werten von a bis b zurück (beides soweit möglich inklusiv). Die Schrittweite beträgt n"
  }

  object slice extends MFunction {
    override val name: String = "slice"
    override val params: Int = 3
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val listFunction = functionPointers.get(param(0).toInt)
      val start = param(1).toInt
      val end = param(2).toInt
      val length = AUtil.getListLength(functionPointers, listFunction)
      if (length < 0 || start < 0 || end < 0 || start >= length || end >= length) {
        Double.NaN
      } else if (start == end) {
        functionPointers.add(new MFunctionLazyList(1, idx => listFunction.result(functionPointers, start)))
      } else if (start < end) {
        functionPointers.add(new MFunctionLazyList(end - start + 1, idx => listFunction.result(functionPointers, idx + start)))
      } else {
        functionPointers.add(new MFunctionLazyList(start - end + 1, idx => listFunction.result(functionPointers, start - idx)))
      }
    }

    override val name2: String = "Listen-Segment"
    override val doc: String = "<tt>slice(l, a, b)</tt> gibt eine Liste mit allen Werten aus der Liste l zurück zwischen den Indizes a und b (beides inklusiv)"
  }

  object reverse extends MFunction {
    override val name: String = "reverse"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val listFunction = functionPointers.get(param(0).toInt)
      val length = AUtil.getListLength(functionPointers, listFunction)
      if (length < 0) {
        Double.NaN
      } else {
        functionPointers.add(new MFunctionLazyList(length, idx => listFunction.result(functionPointers, length - idx - 1)))
      }
    }

    override val name2: String = "Listen-Umkehrung"
    override val doc: String = "Erwartet eine Liste und dreht sie um."
  }

  object sgn extends MFunction {
    override val name: String = "sgn"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = {
      param(0) match {
        case x if x.isNaN => Double.NaN
        case 0 => 0
        case x if x > 0 => 1
        case x if x < 0 => -1
        case _ => Double.NaN
      }
    }

    override val name2: String = "Signum"
    override val doc: String = "Gibt den signum einer zahl zurück. Dieser ist -1 wenn die zahl negativ ist, 0 wenn sie 0 ist und 1 wenn sie positiv ist."
  }

  object dotP extends MFunction {
    override val name: String = "dotP"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val vec1 = AUtil.getList(functionPointers, functionPointers.get(param(0).toInt))
      val vec2 = AUtil.getList(functionPointers, functionPointers.get(param(1).toInt))
      if (vec1 == null || vec2 == null || vec1.size != vec2.size) {
        Double.NaN
      } else {
        var sum = 0d
        for (i <- vec1.indices) {
          sum += (vec1(i) * vec2(i))
        }
        sum
      }
    }

    override val name2: String = "Skalarprodukt"
    override val doc: String = "Gibt das Skalarprodukt von 2 Vektoren (Listen) zurück. Sie müssen gleiche Dimensionen haben."
  }

  object crossP extends MFunction {
    override val name: String = "crossP"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val vec1 = AUtil.getList(functionPointers, functionPointers.get(param(0).toInt))
      val vec2 = AUtil.getList(functionPointers, functionPointers.get(param(1).toInt))
      if (vec1 == null || vec2 == null || vec1.size != vec2.size) {
        Double.NaN
      } else vec1.size match {
        case 3 =>
          //noinspection ZeroIndexToHead
          functionPointers.add(new MFunctionList(List(
            (vec1(1) * vec2(2)) - (vec1(2) * vec2(1)),
            (vec1(2) * vec2(0)) - (vec1(0) * vec2(2)),
            (vec1(0) * vec2(1)) - (vec1(1) * vec2(0))
          )))
        case _ => Double.NaN
      }
    }

    override val name2: String = "Kreuzprodukt"
    override val doc: String = "Gibt das Kreuzprodukt von 2 Vektoren (Listen) zurück. Sie müssen gleiche Dimensionen haben und dürfen keine anderen Dimensionen haben als 3, da das Kreuzprodukt nur dort definiert ist."
  }

  object vecAdd extends MFunction {
    override val name: String = "vecAdd"
    override val params: Int = Parser.VARARG

    override def result(functionPointers: FuncData, param: Double*): Double = {
      var result: ListBuffer[Double] = null
      for (vec <- param.map(_.toInt).map(functionPointers.get).map(AUtil.getList(functionPointers, _))) {
        if (vec == null) {
          return Double.NaN
        } else if (result == null) {
          result = ListBuffer.from(vec)
        } else if (result.size != vec.size && vec.size != 1) {
          return Double.NaN
        } else if (vec.size == 1) {
          for (i <- result.indices) {
            result(i) += vec.head
          }
        } else {
          for (i <- result.indices) {
            result(i) += vec(i)
          }
        }
      }
      functionPointers.add(new MFunctionList(result.toList))
    }

    override val name2: String = "Vektoraddition"
    override val doc: String = "Addiert die gegebenen Vektoren. Sie müssen gleiche Dimensionen haben oder eindimensional sein. In diesem Fall wird der Vektor als n-Dimensionaler Vektor mit allen Werten gleich gehandhabt."
  }

  object vecMul extends MFunction {
    override val name: String = "vecMul"
    override val params: Int = Parser.VARARG

    override def result(functionPointers: FuncData, param: Double*): Double = {
      var result: ListBuffer[Double] = null
      for (vec <- param.map(_.toInt).map(functionPointers.get).map(AUtil.getList(functionPointers, _))) {
        if (vec == null) {
          return Double.NaN
        } else if (result == null) {
          result = ListBuffer.from(vec)
        } else if (result.size != vec.size && vec.size != 1) {
          return Double.NaN
        } else if (vec.size == 1) {
          for (i <- result.indices) {
            result(i) *= vec.head
          }
        } else {
          for (i <- result.indices) {
            result(i) *= vec(i)
          }
        }
      }
      functionPointers.add(new MFunctionList(result.toList))
    }

    override val name2: String = "Vektormultiplikation"
    override val doc: String = "Multipliziert die gegebenen Vektoren. Sie müssen gleiche Dimensionen haben oder eindimensional sein. In diesem Fall wird der Vektor als n-Dimensionaler Vektor mit allen Werten gleich gehandhabt."
  }

  object vecN extends MFunction {
    override val name: String = "vecN"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val vec = AUtil.getList(functionPointers, functionPointers.get(param(0).toInt))
      if (vec == null) {
        Double.NaN
      } else {
        Math.sqrt(vec.map(Math.pow(_, 2)).sum)
      }
    }

    override val name2: String = "Vektorbetrag"
    override val doc: String = "Gibt den Betrag des gegebenen Vektors zurück."
  }

  object vecA extends MFunction {
    override val name: String = "vecA"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val vec = AUtil.getList(functionPointers, functionPointers.get(param(0).toInt))
      if (vec == null || vec.size != 2) {
        Double.NaN
      } else {
        //noinspection ZeroIndexToHead
        Math.atan2(vec(1), vec(0))
      }
    }

    override val name2: String = "Vektorrichtung"
    override val doc: String = "Gibt den Winkel des gegebenen 2-Dimensionalen Vektors zurück. <tt>0°</tt> ist ein Vektor, der nach rechts zeigt und <tt>90°</tt> ist ein Vektor der nach oben zeigt. Der Wert ist im Bodenmaß. Um ein Ergebnis im Gradmaß zu erhalten kann die Funktion <tt>deg</tt> verwendet werden."
  }

  object vecNorm extends MFunction {
    override val name: String = "vecNorm"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val vec = AUtil.getList(functionPointers, functionPointers.get(param(0).toInt))
      if (vec == null) {
        Double.NaN
      } else {
        val mag = Math.sqrt(vec.map(Math.pow(_, 2)).sum)
        functionPointers.add(new MFunctionList(vec.map(_ / mag)))
      }
    }

    override val name2: String = "Vektornormalisierung"
    override val doc: String = "Normalisiert den gegebenen Vektor."
  }

  object fib extends MFunction {
    override val name: String = "fib"
    override val params: Int = 1
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val idx = param.head.toInt
      if (idx <= 0) {
        Double.NaN
      } else if (idx <= 2) {
        1
      } else {
        var last = BigInt(1)
        var last2 = BigInt(1)
        for (_ <- 2 until idx) {
          val tmp = last
          last += last2
          last2 = tmp
        }
        last.toDouble
      }
    }

    override val name2: String = "Fibonacci-Folge"
    override val doc: String = "<tt>fib(n)</tt> gibt das n-te Element der Fibonacci-Folge zurück. Das erste Element hat den Index 1."
  }

  object seq extends MFunction {
    override val name: String = "seq"
    override val params: Int = 2
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val start = param.head
      val func = functionPointers.get(param(1).toInt)
      if (func == null || !AUtil.correctParams(func, 1)) {
        Double.NaN
      } else {
        functionPointers.add(new MFunction {
          private val cache = ListBuffer[Double](start)

          override val name: String = ""
          override val params: Int = 1

          override def result(functionPointers: FuncData, param: Double*): Double = {
            val idx = param.head.toInt - 1
            if (idx < 0) {
              Double.NaN
            } else if (idx < cache.size) {
              cache(idx)
            } else {
              for (i <- cache.size to idx) {
                cache.append(func.result(functionPointers, cache(i - 1)))
              }
              cache(idx)
            }
          }

          override val name2: String = "Folge"
        })
      }
    }

    override val name2: String = "Folge"
    override val doc: String = "Definiert eine Zahlenfolge rekursiv. <tt>seq(2, @[2*x])</tt> gibt einen Funktionspointer zurück für eine Folge mit den Werten 2, 4, 8, 16, ... Der erste Wert hat den Index 1."
  }

  object has extends MFunction {
    override val name: String = "has"
    override val params: Int = Parser.VARARG
    override def result(functionPointers: FuncData, param: Double*): Double = {
      val list = AUtil.getList(functionPointers, functionPointers.get(param(0).toInt))
      if (list == null) {
        Double.NaN
      } else {
        for (i <- param.indices) {
          if (i >= 1 && !list.contains(param(i))) {
            return AUtil.fromBool(false)
          }
        }
        AUtil.fromBool(true)
      }
    }

    override val name2: String = "Enthält"
    override val doc: String = "Prüft ob die Liste (1. Argument) alle anderen Argumente enthält."
  }
  
  object lerp extends MFunction {
    override val name: String = "lerp"
    override val params: Int = 3
    override def result(functionPointers: FuncData, param: Double*): Double = {
      if (param(1) == param(2)) {
        param(1)
      } else if (param(0) == 0) {
        param(1)
      } else if (param(0) == 1) {
        param(2)
      } else if (param(1) * param(2) < 0) {
        ((1 - param(0)) * param(1)) + (param(0) * param(2))
      } else {
        param(1) + (param(0) * (param(2) - param(1)))
      }
    }

    override val name2: String = "Lineare Interpolation"
    override val doc: String = "<tt>lerp(x, a, b)</tt> berechnet Funktionswert einer Geraden durch die Punkte (0 | a) und (1 | b)."
  }
  
  val additionalDocumentation: Set[DocumentationObject] = Set[(String, String, String)](
    (".tuxtr.rc", "Init-Datei", "Die Datei <tt>~/.tuxtr.rc</tt> wird beim Start gelesen. Alle Befehle werden ausgeführt und Variablen werden zu Konstanten, eigene Funktionen zu Systemfunktionen gemacht. Man kann dort folgende zusätzliche Dinge mit <tt>def</tt> definieren (Nach dem hier Abgedruckten folgen wie gewöhnlich Funktionsname und Argumente):\n" +
      "Operatoren: <tt>def op [priority] [isRightAssociative]</tt>\t<tt>[priority]</tt> muss eins der folgenden sein: " + Priority.values().map(_.id).map("<tt>" + _ + "</tt>").dropRight(1).mkString(", ") + " oder <tt>" + Priority.values().last.id + "</tt>. [isRightAssociative] muss entweder <tt>true</tt> oder <tt>false</tt> sein. 2 Argumente\n" +
      "Vorzeichen: <tt>def unary</tt>\t1 Argument\n" +
      "Nachgestellte Operatoren: <tt>def postfix</tt>\t1 Argument"),
    (".tuxtr.rc", "LUA", "In der Datei <tt>~/.tuxtr.rc</tt> kann mit <tt>__LUA__</tt> LUA-Code eingeleitet werden. Dieser geht dann bis zum Ende der Datei. Er wird zuerst ausgeführt, sodass andere Definitionen auf die hier definierten Funktionen zugreifen können. Um eine Funktion im Taschenrechner zugreifbar zu machen, muss eine Liste namens <tt>EXPORT</tt> definiert werden. Diese kann nun Funktionsdefinitionen enthalten. Eine Funktionsdefinition ist eine List bestehend aus einem String (Funktionsname), einer LUA-Funktion (auszuführede Funktion), einer ganzen Zahl (Parameteranzahl) und optional einem String, der als Dokumentation angezeigt wird. Eine in LUA-definierte Funktion die exportiert wird, erhält als erstes Argument, also vor den anderen ein <tt>Userdata</tt>. Dieses erlaubt den Zugriff auf Funktionspointer. Folgende Dinge sind bereits definiert:\n" +
      "VARARG  \tDieser Wert sollte als Parameteranzahl für Vararg-Funktionen verwendet werden.\n" +
      "getFunc  \tErwartet als erstes Argument einen <tt>FunctionPointers</tt> Userdata und als zweites Argument eine ganze Zahl (Den Pointer). Gibt die Funktion, auf die der Pointer zeigt als LUA-Funktion zurück, sodass sie aufgerufen werden kann. Gibt außerdem als zweiten Rückgabewert die Parameteranzahl der Funktion zurück. Ist der Pointer ungültig, wird zwei mal <tt>nil</tt> zurückgegeben. Die zurückgegebene Funktion erwartet kein <tt>FunctionPointers</tt>-Userdata als erstes Argument.\n" +
      "addFunc  \tErwartet als erstes Argument einen <tt>FunctionPointers</tt> Userdata, als zweites Argument eine LUA-Funktion und als drittes Argument eine ganze Zahl (Parameteranzahl). Fügt die Funktion dem Taschenrechner hinzu und gibt einen Pointer darauf zurück. Optional kann ein boolean-Wert angehängt werden, der angibt, ob der gegebenen Funktion ein <tt>FunctionPointers</tt>-Userdata übergeben werden soll. Standard ist <tt>true</tt>.\n" +
      "getList  \tErwartet als erstes Argument einen <tt>FunctionPointers</tt> Userdata und als zweites Argument eine ganze Zahl (Den Pointer). Gibt eine Lua-Liste zurück, die die von der Funktion repräsentiert wird.\n" +
      "addList  \tErwartet als erstes Argument einen <tt>FunctionPointers</tt> Userdata, als zweites Argument eine LUA-Liste Fügt die Liste dem Taschenrechner hinzu und gibt einen Pointer zurück.\n" +
      "toBool  \tErwartet eine Zahl und gibt ihre boolean-Represäntation zurück. Wenn ein boolescher Wert erwartet wird, sollte diese Funktion aufgerufen werden.\n" +
      "fromBool  \tErwartet einen booleschen Wert und gibt die Zahlenrepresäntation zurück. Wenn ein boolescher Wert zurückgegeben wird, sollte diese Funktion verwendet werden.\n" +
      "adjustBool  \tErwartet eine Zahl und gibt die Zahlenrepresäntation des booleans zurück. Wenn ein boolescher Wert in Zahlenrepresäntation gespeichert ist, sollte diese Funktion aufgerufen werden, bevor er zurückgegeben wird."),
    ("let", "Variablendeklaration", "<tt>let x = 4 + 6</tt> setzt die Variable x auf 10. Sie kann danach wie eine Konstante verwendet werden. Variablen können Konstanten überschreiben."),
    ("def", "Definieren", "Definiert eine Funktion. Beispiel: <tt>def f(x) = 2 * x + 4</tt>. Dabei wird der aktuelle Stand des Parsers als Closure gespeichert (Momentan definierte Variablen werden im auszuwertenden Term zu Konstanten, nachträgliche Umdefinitionen wirken sich nicht darauf aus.) Diese Methode kann eingebaute Funktionen umdefinieren. Funktionen können sich nicht selbst aufrufen, da das Closure erstellt wird bevor die Funktion dem Parser hinzugefügt wird."),
    ("defp", "Pointer-Definieren", "Definiert eine Funktion. Beispiel: <tt>defp f = @sin</tt>. Dabei wird <tt>f(x)=sin(x)</tt> definiert."),
    ("list...", "Argument-Splatting", "Bei einem Funktionsaufruf können an einen Listen-Pointer 3 Punkte angehängt werden. Dann wird nicht der Pointer an die Funktion übergeben, sondern die Liste aufgelöst und jedes Element der Liste als einzelnes Argument übergeben. Ein Argument mit 3 Punkten ist also unter Umständen mehrere Argumente wert."),
    ("clear", "Zurücksetzen", "Setzt einen Wert zurück. Löscht alle Variablen unter diesem Namen, macht alle <tt>undef</tt> Aufrufe mit diesem Namen rückgängig und löscht alle eigenen Funktionen unter diesem Namen. Ausgenommen sind Definitionen aus <tt>.tuxtr.rc</tt>. Beispiel: <tt>clear x</tt>"),
    ("clearall", "Alles Zurücksetzten", "Setzt alles zurück. Löscht alle Variablen, macht alle Aufrufe von <tt>undef</tt> Rückgängig und löscht alle eigenen Funktionen. Ausgenommen sind Definitionen aus <tt>.tuxtr.rc</tt>."),
    ("undef", "Undefinieren", "Macht diesen Namen unverwendbar. Nach einem Aufruf von <tt>undef x</tt> können Variablen, Konstanten, Funktionen, Operatoren, Vorzeichen und Nachgestellte Operatoren mit dem Namen x nicht mehr verwendet werden. Mit <tt>undef +</tt> würde der Ausdruck <tt>1 + 1</tt> eine Fehlermeldung produzieren."),
    ("`op`", "Operatoreschreibweise", "Eigene Operatoren, die in <tt>.tuxtr.rc</tt> definiert sind und nicht das Zeichen eines Standardoperators verwenden müsen in Backticks (`) geschrieben werden."),
    ("@func#params", "Funktionsreferenzen", "<tt>@func#params</tt> gibt einen Pointer auf die Funktion <tt>func</tt> mit <tt>params</tt> Argumenten zurück. Wird Das <tt>#params</tt> weggelassen, wird die Funktion mit den wenigsten Argumenten gewählt. Vararg Funktionen werden als erstes gewählt. Um eine Vararg- Funktion zu erhalten sollte das <tt>#params</tt> deswegen weggelassen werden."),
    ("@[a, b = a + b]", "Anonyme Funktionsreferenzen", "<tt>@[a, b = a + b]</tt> erstellt einen anonyme Funktion (Funktion ohne Namen) die zwei Argumente addiert und gibt einen Pointer darauf zurück. Ein Spezialfall: <tt>@[]</tt> gibt einen Pointer auf eine Funktion zurück, die ein Argument erwartet und genau dieses zurückgibt."),
    ("list(1, 2, 3)", "Listen", "Listen sind Funktionen, die zu ganzen Zahlen von 0 bis (1 - ihre Länge) konstante Werte zurückgeben und für -1 ihre Länge zurückgeben. Listen können mit der <tt>list</tt> Funktion erstellt werden."),
    ("n? t? f? l?", "Anzeige-Modi", "Es gibt vire Anzeigemodi die angeben, wie ein Ergebnis angezeigt werden soll. Sie können mit <tt>modi?</tt> vor dem Term eingestellt werden. Der Standard ist number.\n" +
      "number? oder n?:  \tZeigt das Ergebnis als Zahl an\n" +
      "text? oder t?:  \tZeigt das Ergebnis als Textrepräsentation einer Zahl an\n" +
      "function?, func? oder f?:  \tInterpretiert das Ergebnis als Funktionspointer und Zeigt Informationen zu diesem an.\n" +
      "list? oder l?:  \tInterpretiert das Ergebnis als Liste und zeigt eine Liste an.")
  ).map(str => new DocumentationObject {
    override val name: String = str._1
    override val name2: String = str._2
    override val dtype: DocType = DocType.GENERAL
    override val doc: String = str._3
  })
}
