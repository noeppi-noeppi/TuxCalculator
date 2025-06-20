package tuxcalculator.core.data

import tuxcalculator.core.Calculator
import tuxcalculator.core.lexer.{CatCode, Lexer, RemainingText}
import tuxcalculator.core.util.Util

object CalculatorCommands {
  
  private val BaseCommands: Set[String] = Set("let", "def", "rem", "set", "cat")
  private val IniCommands: Set[String] = BaseCommands | Set("dump")
  private val AssignmentCommands: Set[String] = Set("let", "def", "set", "cat")
  
  def commands(calc: Calculator): Set[String] = if (calc.ini) IniCommands else BaseCommands
  def isAssignmentCommand(cmd: String): Boolean = AssignmentCommands.contains(cmd)
}

class CalculatorCommands(private val lexer: Lexer) {

  class Command(val name: String) {
    private[this] val sub: SubCommand = new SubCommand(name)
    def unapply(string: String): Option[RemainingText] = sub.unapply(RemainingText(string, 0))
  }
  
  class SubCommand(val name: String) {
    def unapply(text: RemainingText): Option[RemainingText] = {
      val decomposed = Util.decomposeString(text.string)
      val skippedSpace = decomposed.takeWhile(codePoint => lexer.catCode(codePoint) == CatCode.Space).size
      val normString: String = Util.makeString(decomposed.dropWhile(codePoint => lexer.catCode(codePoint) == CatCode.Space))
      if (!normString.startsWith(name)) return None
      val remaining = normString.substring(name.length)
      if (remaining.isEmpty) return Some(RemainingText(remaining, skippedSpace + name.length))
      val catCode: CatCode = lexer.catCode(remaining.codePointAt(0))
      if (catCode == CatCode.Letter || catCode == CatCode.Digit || catCode == CatCode.Exp) return None
      Some(RemainingText(remaining, text.offset + skippedSpace + name.length))
    }
  }

  val Let: Command = new Command("let")
  val Def: Command = new Command("def")
  val Rem: Command = new Command("rem")
  val Set: Command = new Command("set")
  val Cat: Command = new Command("cat")
  val Dump: Command = new Command("dump")
  
  val Fmt: SubCommand = new SubCommand("fmt")
}
