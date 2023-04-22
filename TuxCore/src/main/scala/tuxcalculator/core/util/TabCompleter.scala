package tuxcalculator.core.util

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorCommands
import tuxcalculator.core.lexer.CatCode
import tuxcalculator.core.lexer.CatCode.CatCode

import java.text.Normalizer
import java.util.Locale

object TabCompleter {
  
  private val Identifier: Set[CatCode] = Set(CatCode.Letter, CatCode.Digit, CatCode.Exp)
  private val Reference: Set[CatCode] = Identifier | Set(CatCode.Sign, CatCode.Operator, CatCode.Post, CatCode.Assign)

  def tabComplete(calc: Calculator, content: String): Result = {
    val codePoints = Util.decomposeString(content)
    
    def findPrefix(catcodes: Set[CatCode], startsWith: Option[CatCode]): Option[Prefix] = {
      val matchIdx = codePoints.lastIndexWhere(codePoint => !catcodes.contains(calc.lexer.catCode(codePoint)))
      val spaceSkipped = codePoints.lastIndexWhere(codePoint => calc.lexer.catCode(codePoint) != CatCode.Space, matchIdx)
      startsWith match {
        case Some(startCode) if spaceSkipped < 0 || calc.lexer.catCode(codePoints(spaceSkipped)) != startCode => None
        case _ => Some(Prefix(Util.makeString(codePoints.take(spaceSkipped + 1)), Util.makeString(codePoints.drop(spaceSkipped + 1)), Util.makeString(codePoints.drop(matchIdx + 1))))
      }
    }
    
    def findMatches(all: Set[String], matchString: String): Vector[String] = {
      def normalized(string: String): String = Normalizer.normalize(string, Normalizer.Form.NFKD)
          .replaceAll("\\p{M}", "").toLowerCase(Locale.ROOT)
      val normalizedMatch = normalized(matchString)
      all.filter(str => normalized(str).startsWith(normalizedMatch)).toVector.sortBy(normalized)
    }
    
    findPrefix(Identifier, Some(CatCode.Special)) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        return Result(prefix, completionString, findMatches(calc.specials.keys, matchString), isIdentifier = false)
      case None =>
    }
    
    findPrefix(Reference, Some(CatCode.Reference)) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        return Result(prefix, completionString, findMatches(calc.resolution.tabCompleteReference, matchString), isIdentifier = false)
      case None =>
    }
    
    findPrefix(Identifier, None) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        val baseMatches: Vector[String] = findMatches(calc.resolution.tabCompleteIdentifier, matchString)
        val matches: Vector[String] = if (prefix.isEmpty) {
          val commands: Set[String] = CalculatorCommands.commands(calc)
          val commandMatches: Vector[String] = findMatches(commands, matchString)
          commandMatches ++ baseMatches.filter(str => !commands.contains(str))
        } else {
          baseMatches
        }
        return Result(prefix, completionString, matches, isIdentifier = true)
      case None =>
    }
    
    Result(content, "", Vector(), isIdentifier = false)
  }
  
  case class Result(prefix: String, completionString: String, matches: Vector[String], isIdentifier: Boolean)
  private case class Prefix(prefix: String, completionString: String, matchString: String)
}
