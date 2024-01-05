package tuxcalculator.core.util

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.{CalculatorCommands, CalculatorProperties}
import tuxcalculator.core.lexer.CatCode

import java.text.Normalizer
import java.util.Locale

object TabCompleter {
  
  private val TightCatCodes: Set[CatCode] = Set(CatCode.Reference, CatCode.Special)
  private val Identifier: Set[CatCode] = Set(CatCode.Letter, CatCode.Digit, CatCode.Exp)
  private val Reference: Set[CatCode] = Identifier | Set(CatCode.Sign, CatCode.Operator, CatCode.Post, CatCode.Assign)

  def tabComplete(calc: Calculator, line: String): Result = {
    val codePoints = Util.decomposeString(line)
    
    def findPrefix(catcodes: Set[CatCode], startsWith: Option[CatCode]): Option[Prefix] = {
      val matchIdx = codePoints.lastIndexWhere(codePoint => !catcodes.contains(calc.lexer.catCode(codePoint)))
      val spaceSkipped = codePoints.lastIndexWhere(codePoint => calc.lexer.catCode(codePoint) != CatCode.Space, matchIdx)
      startsWith match {
        case Some(startCode) if spaceSkipped < 0 || calc.lexer.catCode(codePoints(spaceSkipped)) != startCode => None
        case _ =>
          // If there is a start cat code or we follow a tight cat code, skip the entire space.
          // Otherwise keep a single space if present
          val spaceToSkip = if ((matchIdx - spaceSkipped) <= 0 || startsWith.isDefined || TightCatCodes.contains(calc.lexer.catCode(codePoints(spaceSkipped)))) spaceSkipped else spaceSkipped + 1
          Some(Prefix(Util.makeString(codePoints.take(spaceToSkip + 1)), Util.makeString(codePoints.drop(spaceToSkip + 1)), Util.makeString(codePoints.drop(matchIdx + 1))))
      }
    }
    
    def findMatches(all: Set[String], matchString: String): Vector[String] = {
      def normalized(string: String): String = Normalizer.normalize(string, Normalizer.Form.NFKD)
          .replaceAll("\\p{M}", "").toLowerCase(Locale.ROOT)
      val normalizedMatch = normalized(matchString)
      all.filter(str => normalized(str).startsWith(normalizedMatch)).toVector.sortBy(normalized)
    }
    
    // Calculator properties
    findPrefix(Identifier, None) match {
      case Some(Prefix(prefix, completionString, matchString)) if prefix.strip() == "set" =>
        return Result(prefix, completionString, findMatches(CalculatorProperties.allProperties, matchString), isIdentifier = false)
      case _ =>
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
    
    Result(line, "", Vector(), isIdentifier = false)
  }
  
  case class Result(prefix: String, completionString: String, matches: Vector[String], isIdentifier: Boolean)
  private case class Prefix(prefix: String, completionString: String, matchString: String)
}
