package tuxcalculator.core.expression

import org.apache.commons.text.StringEscapeUtils
import tuxcalculator.core.Calculator
import tuxcalculator.core.function.Descriptor
import tuxcalculator.core.lexer.FmtCode
import tuxcalculator.core.value.MathValue

object Ast {
  
  sealed trait Command {
    def string(calc: Calculator): String
  }
  
  case class LetCommand(name: String) extends Command {
    def string(calc: Calculator): String = "let " + name + ""
  }
  
  case class DefCommand(target: DefTarget, priority: Option[Expression], sig: Signature) extends Command {
    def string(calc: Calculator): String = "def" + (if (priority.isDefined) calc.format(FmtCode.Open) + priority.get.string(calc) + calc.format(FmtCode.Close) else "") + " " + target.toString + calc.format(FmtCode.Open) + sig + calc.format(FmtCode.Close)
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
  
  case class SetFmtCommand(name: String) extends Command {
    def string(calc: Calculator): String = "set fmt " + name
  }
  
  case class DumpCommand(fileName: String) extends Command {
    def string(calc: Calculator): String = "dump '" + StringEscapeUtils.escapeJava(fileName) + "'"
  }
  
  sealed trait PartialArgument {
    def string(calc: Calculator): String
  }
  
  case object Placeholder extends PartialArgument {
    override def string(calc: Calculator): String = calc.format(FmtCode.Partial)
  }
  
  sealed trait Argument extends PartialArgument {
    def string(calc: Calculator): String
  }
  
  case class SplattedArgument(expr: Expression) extends Argument {
    override def string(calc: Calculator): String = expr.string(calc) + calc.format(FmtCode.VarArg)
  }
  
  sealed trait Expression extends Argument {
    def string(calc: Calculator): String
  }

  case class Group(value: Expression) extends Expression {
    override def string(calc: Calculator): String = calc.format(FmtCode.Open) + value.string(calc) + calc.format(FmtCode.Close)
  }

  case object Answer extends Expression {
    override def string(calc: Calculator): String = calc.format(FmtCode.Answer)
  }

  case class Variable(name: String) extends Expression {
    override def string(calc: Calculator): String = name
  }

  case class Value(value: MathValue) extends Expression {
    override def string(calc: Calculator): String = calc.format(value)
  }
  
  case class Error(head: String, tail: Vector[Error.TailPart]) extends Expression {
    override def string(calc: Calculator): String = calc.format(FmtCode.Error) + calc.escapeErrorLiteral(head) + tail.map(entry => entry.string(calc)).mkString + calc.format(FmtCode.Error)
  }
  
  object Error {
    case class TailPart(prefix: String, variableName: String, followingText: String) {
      def string(calc: Calculator): String = prefix + variableName + " " + calc.escapeErrorLiteral(followingText)
    }
  }

  case class Reference(target: DefTarget) extends Expression {
    override def string(calc: Calculator): String = calc.format(FmtCode.Reference) + target.toString
  }

  case class Special(name: String) extends Expression {
    override def string(calc: Calculator): String = calc.format(FmtCode.Special) + name
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
    override def string(calc: Calculator): String = calc.format(FmtCode.Lambda) + sig + calc.format(FmtCode.Follow) + definitionCode.string(calc)
  }
  object Lambda {
    def apply(sig: Signature, code: Expression): Lambda = Lambda(sig, code, code)
  }
  
  case class Match(entries: Vector[MatchEntry]) extends Expression {
    override def string(calc: Calculator): String = calc.format(FmtCode.StartMatch) + entries.map(_.string(calc)).mkString(calc.format(FmtCode.GroupSep)) + calc.format(FmtCode.EndMatch)
  }
  
  case class MatchEntry(sig: Signature, elementGuards: Vector[Option[DefExpression]], mainGuard: Option[DefExpression], code: Expression, definitionCode: Expression) {
    if (sig.names.length != elementGuards.length) throw new IllegalArgumentException("Guard count does not match signature. This is a bug.")
    def string(calc: Calculator): String = {
      val namesWithVararg = if (sig.vararg && sig.names.nonEmpty) sig.names.init ++ Vector(sig.names.last + calc.format(FmtCode.VarArg)) else sig.names
      val argsString = (namesWithVararg zip elementGuards).map(e => e._1 + e._2.map(expr => calc.format(FmtCode.Guard) + expr.definitionCode.string(calc)).getOrElse("")).mkString(calc.format(FmtCode.ElementSep))
      calc.format(FmtCode.Open) + argsString + calc.format(FmtCode.Close) + mainGuard.map(expr => calc.format(FmtCode.Guard) + expr.definitionCode.string(calc)).getOrElse("") + calc.format(FmtCode.Follow) + definitionCode.string(calc)
    }
  }

  case class Invocation(name: String, args: Vector[PartialArgument]) extends Expression {
    override def string(calc: Calculator): String = name + calc.format(FmtCode.Open) + args.map(_.string(calc)).mkString(calc.format(FmtCode.ElementSep)) + calc.format(FmtCode.Close)
  }
  
  case class PartialInvocation(name: String, args: Vector[PartialArgument]) extends Expression {
    override def string(calc: Calculator): String = name + calc.format(FmtCode.Partial) + calc.format(FmtCode.Open) + args.map(_.string(calc)).mkString(calc.format(FmtCode.ElementSep)) + calc.format(FmtCode.Close)
  }
  
  case class ShorthandInvocation(name: String, partialArgs: Vector[Ast.Expression], arg: Expression) extends Expression {
    override def string(calc: Calculator): String = {
      if (partialArgs.isEmpty) {
        name + " " + arg.string(calc)
      } else {
        name + calc.format(FmtCode.Partial) + calc.format(FmtCode.Open) + partialArgs.map(_.string(calc)).mkString(calc.format(FmtCode.ElementSep)) + calc.format(FmtCode.Close) + calc.format(FmtCode.Open) + arg.string(calc) + calc.format(FmtCode.Close)
      }
    }
  }
  
  case class Application(value: Expression, args: Vector[PartialArgument]) extends Expression {
    override def string(calc: Calculator): String = value.string(calc) + calc.format(FmtCode.Open) + args.map(_.string(calc)).mkString(calc.format(FmtCode.ElementSep)) + calc.format(FmtCode.Close)
  }
  
  case class PartialApplication(value: Expression, args: Vector[PartialArgument]) extends Expression {
    override def string(calc: Calculator): String = value.string(calc) + calc.format(FmtCode.Partial) + calc.format(FmtCode.Open) + args.map(_.string(calc)).mkString(calc.format(FmtCode.ElementSep)) + calc.format(FmtCode.Close)
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
