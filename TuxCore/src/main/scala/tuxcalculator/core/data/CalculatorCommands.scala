package tuxcalculator.core.data

import tuxcalculator.core.Calculator
import tuxcalculator.core.lexer.{CatCode, Lexer, RemainingText}
import tuxcalculator.core.util.Util

object CalculatorCommands {
  
  private val BaseCommands: Set[String] = Set("let", "def", "rem", "set", "cat", "tok")
  private val IniCommands: Set[String] = BaseCommands | Set("dump")
  private val AssignmentCommands: Set[String] = Set("let", "def", "set", "cat", "tok")
  
  def commands(calc: Calculator): Set[String] = if (calc.ini) IniCommands else BaseCommands
  def isAssignmentCommand(cmd: String): Boolean = AssignmentCommands.contains(cmd)
}

class CalculatorCommands(private val lexer: Lexer) {

  case class Command(cmd: String) {
    def unapply(string: String): Option[RemainingText] = {
      val decomposed = Util.decomposeString(string)
      val skippedSpace = decomposed.takeWhile(codePoint => lexer.catCode(codePoint) == CatCode.Space).size
      val normString: String = Util.makeString(decomposed.dropWhile(codePoint => lexer.catCode(codePoint) == CatCode.Space))
      if (!normString.startsWith(cmd)) return None
      val remaining = normString.substring(cmd.length)
      if (remaining.isEmpty) return Some(RemainingText(remaining, skippedSpace + cmd.length))
      val catCode: CatCode = lexer.catCode(remaining.codePointAt(0))
      if (catCode == CatCode.Letter || catCode == CatCode.Digit || catCode == CatCode.Exp) return None
      Some(RemainingText(remaining, skippedSpace + cmd.length))
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
