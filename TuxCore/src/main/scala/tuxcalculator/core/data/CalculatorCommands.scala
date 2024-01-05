package tuxcalculator.core.data

import tuxcalculator.core.Calculator
import tuxcalculator.core.lexer.{CatCode, Lexer}
import tuxcalculator.core.util.Util

import java.util.Locale

object CalculatorCommands {
  
  private val BaseCommands: Set[String] = Set("let", "def", "rem", "set", "cat", "tok")
  private val IniCommands: Set[String] = BaseCommands | Set("dump")
  private val AssignmentCommands: Set[String] = Set("let", "def", "set", "cat", "tok")
  
  def commands(calc: Calculator): Set[String] = if (calc.ini) IniCommands else BaseCommands
  def isAssignmentCommand(cmd: String): Boolean = AssignmentCommands.contains(cmd)
}

class CalculatorCommands(private val lexer: Lexer) {

  case class Command(cmd: String) {
    def unapply(string: String): Option[String] = {
      val normString: String = Util.makeString(Util.decomposeString(string).dropWhile(codePoint => lexer.catCode(codePoint) == CatCode.Space))
      if (!normString.toLowerCase(Locale.ROOT).startsWith(cmd)) return None
      val remaining = normString.substring(cmd.length)
      if (remaining.isEmpty) return Some(remaining)
      val catCode: CatCode = lexer.catCode(remaining.codePointAt(0))
      if (catCode == CatCode.Letter || catCode == CatCode.Digit || catCode == CatCode.Exp) return None
      Some(remaining)
    }
  }

  val Let: Command = Command("let")
  val Def: Command = Command("def")
  val Rem: Command = Command("rem")
  val Set: Command = Command("set")
  val Cat: Command = Command("cat")
  val Tok: Command = Command("tok")
  val Dump: Command = Command("dump")
}
