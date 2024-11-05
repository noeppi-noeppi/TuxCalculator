package tuxcalculator.core.expression

import org.apache.commons.text.StringEscapeUtils
import tuxcalculator.core.Calculator
import tuxcalculator.core.function.Descriptor
import tuxcalculator.core.value.MathValue

object Ast {
  
  sealed trait Command {
    def string(calc: Calculator): String
  }
  
  case class LetCommand(name: String) extends Command {
    def string(calc: Calculator): String = "let " + name + ""
  }
  
  case class DefCommand(target: DefTarget, priority: Option[Expression], sig: Signature) extends Command {
    def string(calc: Calculator): String = "def" + (if (priority.isDefined) "[" + priority.get.string(calc) + "]" else "") + " " + target.toString + "(" + sig + ")"
  }
  
  sealed trait DefTarget {
    val name: String
    override def toString: String = name
  }
  object DefTarget {
    case class Function(name: String) extends DefTarget
    case class Operator(name: String) extends DefTarget
    case class SignOrOperator(name: String) extends DefTarget
    case class Post(name: String) extends DefTarget
    case class PrimaryBracket(name: String, close: String) extends DefTarget {
      override def toString: String = name + close
    }
    case class SecondaryBracket(name: String, close: String) extends DefTarget {
      override def toString: String = name + close
    }
    case class TertiaryBracket(name: String, close: String) extends DefTarget {
      override def toString: String = name + close
    }
  }
  
  case class RemCommand(target: DefTarget) extends Command {
    def string(calc: Calculator): String = "rem " + target.toString
  }
  
  case class SetCommand(name: String) extends Command {
    def string(calc: Calculator): String = "set " + name
  }
  
  case class CatCommand(codePoint: Int) extends Command {
    def string(calc: Calculator): String = "cat '" + StringEscapeUtils.escapeJava(Character.toString(codePoint)) + "'"
  }
  
  case class TokCommand(token: String) extends Command {
    def string(calc: Calculator): String = "tok '" + StringEscapeUtils.escapeJava(token) + "'"
  }
  
  case class DumpCommand(fileName: String) extends Command {
    def string(calc: Calculator): String = "dump '" + StringEscapeUtils.escapeJava(fileName) + "'"
  }
  
  sealed trait PartialArgument {
    def string(calc: Calculator): String
  }
  
  case object Placeholder extends PartialArgument {
    override def string(calc: Calculator): String = "_"
  }
  
  sealed trait Argument extends PartialArgument {
    def string(calc: Calculator): String
  }
  
  case class SplattedArgument(expr: Expression) extends Argument {
    override def string(calc: Calculator): String = expr.string(calc) + "..."
  }
  
  sealed trait Expression extends Argument {
    def string(calc: Calculator): String
  }

  case class Group(value: Expression) extends Expression {
    override def string(calc: Calculator): String = "(" + value.string(calc) + ")"
  }

  case object Answer extends Expression {
    override def string(calc: Calculator): String = "answer"
  }

  case class Variable(name: String) extends Expression {
    override def string(calc: Calculator): String = name
  }

  case class Value(value: MathValue) extends Expression {
    override def string(calc: Calculator): String = calc.format(value)
  }
  
  case class Error(head: String, tail: Vector[Error.TailPart]) extends Expression {
    override def string(calc: Calculator): String = "'" + head + tail.map(entry => entry.toString).mkString + "'"
  }
  
  object Error {
    case class TailPart(prefix: String, variableName: String, followingText: String) {
      override def toString: String = prefix + variableName + " " + followingText
    }
  }

  case class Reference(target: DefTarget) extends Expression {
    override def string(calc: Calculator): String = "@" + target.toString
  }

  case class Special(name: String) extends Expression {
    override def string(calc: Calculator): String = "#" + name
  }

  case class PrimaryBracket(open: String, close: String, value: Expression) extends Expression {
    override def string(calc: Calculator): String = open + value + close
  }

  case class SecondaryBracket(open: String, close: String, values: Vector[Argument]) extends Expression {
    override def string(calc: Calculator): String = open + values.map(_.string(calc)).mkString(", ") + close
  }

  case class TertiaryBracket(open: String, close: String, values: Vector[Vector[Expression]]) extends Expression {
    override def string(calc: Calculator): String = open + values.map(col => col.map(_.string(calc)).mkString(", ")).mkString(" ; ") + close
  }

  case class Lambda(sig: Signature, code: Expression, definitionCode: Expression) extends Expression {
    override def string(calc: Calculator): String = "\\" + sig + " -> " + definitionCode.string(calc)
  }
  object Lambda {
    def apply(sig: Signature, code: Expression): Lambda = Lambda(sig, code, code)
  }
  
  case class Match(entries: Vector[MatchEntry]) extends Expression {
    override def string(calc: Calculator): String = "\\[" + entries.map(_.string(calc)).mkString(" ; ") + "]"
  }
  
  case class MatchEntry(sig: Signature, elementGuards: Vector[Option[DefExpression]], mainGuard: Option[DefExpression], code: Expression, definitionCode: Expression) {
    if (sig.names.length != elementGuards.length) throw new IllegalArgumentException("Guard count does not match signature. This is a bug.")
    def string(calc: Calculator): String = {
      val namesWithVararg = if (sig.vararg && sig.names.nonEmpty) sig.names.init ++ Vector(sig.names.last + "...") else sig.names
      val argsString = (namesWithVararg zip elementGuards).map(e => e._1 + e._2.map(expr => ": " + expr.definitionCode.string(calc)).getOrElse("")).mkString(", ")
      "(" + argsString + ")" + mainGuard.map(expr => ": " + expr.definitionCode.string(calc)).getOrElse("") + " -> " + definitionCode.string(calc)
    }
  }

  case class Invocation(name: String, args: Vector[PartialArgument]) extends Expression {
    override def string(calc: Calculator): String = name + "(" + args.map(_.string(calc)).mkString(", ") + ")"
  }
  
  case class PartialInvocation(name: String, args: Vector[PartialArgument]) extends Expression {
    override def string(calc: Calculator): String = name + "_(" + args.map(_.string(calc)).mkString(", ") + ")"
  }
  
  case class ShorthandInvocation(name: String, partialArgs: Vector[Ast.Expression], arg: Expression) extends Expression {
    override def string(calc: Calculator): String = {
      if (partialArgs.isEmpty) {
        name + " " + arg.string(calc)
      } else {
        name + "_(" + partialArgs.map(_.string(calc)).mkString(", ") + ")(" + arg.string(calc) + ")"
      }
    }
  }
  
  case class Application(value: Expression, args: Vector[PartialArgument]) extends Expression {
    override def string(calc: Calculator): String = value.string(calc) + "(" + args.map(_.string(calc)).mkString(", ") + ")"
  }
  
  case class PartialApplication(value: Expression, args: Vector[PartialArgument]) extends Expression {
    override def string(calc: Calculator): String = value.string(calc) + "_(" + args.map(_.string(calc)).mkString(", ") + ")"
  }

  case class SignApplication(operator: String, value: Expression) extends Expression {
    override def string(calc: Calculator): String = operator + value.string(calc)
  }

  case class PostApplication(operator: String, value: Expression) extends Expression {
    override def string(calc: Calculator): String = value.string(calc) + operator
  }

  case class OperatorApplication(head: Expression, tail: Vector[(String, Expression)]) extends Expression {
    override def string(calc: Calculator): String = head.string(calc) + tail.map(entry => " " + entry._1 + " " + entry._2.string(calc)).mkString
  }

  case class Signature(names: Vector[String], vararg: Boolean) {
    lazy val descriptor: Descriptor = Descriptor(names.length, if (names.isEmpty) false else vararg)
    override def toString: String = names.mkString(",") + (if (vararg) "..." else "")
  }
  object Signature {
    val Empty: Signature = new Signature(Vector(), false)
  }
  
  case class DefExpression(code: Expression, definitionCode: Expression)
}
