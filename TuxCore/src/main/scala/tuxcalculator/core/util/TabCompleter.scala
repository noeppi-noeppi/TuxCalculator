package tuxcalculator.core.util

import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.translate.{AggregateTranslator, LookupTranslator}
import tuxcalculator.core.Calculator
import tuxcalculator.core.data.{CalculatorCommands, CalculatorProperties}
import tuxcalculator.core.lexer.{CatCode, CharacterMapping, Lookahead, TokResult}

import java.text.Normalizer
import java.util.Locale
import scala.collection.Set
import scala.jdk.CollectionConverters._

object TabCompleter {
  
  private val TightCatCodes: Set[CatCode] = Set(CatCode.Reference, CatCode.Special)
  private val Identifier: Set[CatCode] = Set(CatCode.Letter, CatCode.Digit, CatCode.Exp)
  private val Reference: Set[CatCode] = Identifier | Set(CatCode.Sign, CatCode.Operator, CatCode.Post, CatCode.Assign)
  private val JustSpace: Set[CatCode] = Set(CatCode.Space)
  private val SpacedIdentifier: Set[CatCode] = Identifier | JustSpace

  def tabComplete(calc: Calculator, line: String): Result = {
    val codePoints = Util.decomposeString(line)
    
    lazy val escapingDelimiter: Option[String] = calc.lexer.escapeCodePoints.minOption.map(Character.toString)
    lazy val escapingTranslator = escapingDelimiter match {
      case Some(delim) => new AggregateTranslator(new LookupTranslator(Map[CharSequence, CharSequence](delim -> ("\\" + delim)).asJava), StringEscapeUtils.ESCAPE_JAVA)
      case None => StringEscapeUtils.ESCAPE_JAVA
    }
    
    def findPrefix(catcodes: Set[CatCode], startsWith: CatCode = null, innerCatcodes: Set[CatCode] = Set()): Option[Prefix] = {
      val matchIdx = codePoints.lastIndexWhere(codePoint => !catcodes.contains(calc.lexer.catCode(codePoint))) match {
        case -1 => -1
        // Inner catcodes may not on the beginning (on the end they are fine though).
        // Treat inner catcodes at the start of the match string as not belonging to the match string
        case outerMatchIdx => codePoints.indexWhere(codePoint => !innerCatcodes.contains(calc.lexer.catCode(codePoint)), outerMatchIdx + 1) match {
          case -1 => codePoints.length - 1 // If all catcodes are inner catcodes, behave as if we have an empty match string
          case idx => idx - 1
        }
      }
      val spaceSkipped = codePoints.lastIndexWhere(codePoint => calc.lexer.catCode(codePoint) != CatCode.Space, matchIdx)
      Option(startsWith) match {
        case Some(startCode) if spaceSkipped < 0 || calc.lexer.catCode(codePoints(spaceSkipped)) != startCode => None
        case startOption =>
          // If there is a start cat code or we follow a tight cat code, skip the entire space.
          // Otherwise keep a single space if present
          val spaceToSkip = if ((matchIdx - spaceSkipped) <= 0 || startOption.isDefined || TightCatCodes.contains(calc.lexer.catCode(codePoints(spaceSkipped)))) spaceSkipped else spaceSkipped + 1
          Some(Prefix(Util.makeString(codePoints.take(spaceToSkip + 1)), Util.makeString(codePoints.drop(spaceToSkip + 1)), Util.makeString(codePoints.drop(matchIdx + 1))))
      }
    }
    
    def findMatches(all: Set[String], matchString: String): Vector[String] = {
      def normalized(string: String): String = Normalizer.normalize(string, Normalizer.Form.NFKD)
          .replaceAll("\\p{M}", "").toLowerCase(Locale.ROOT)
      val normalizedMatch = normalized(matchString)
      all.filter(str => normalized(str).startsWith(normalizedMatch)).toVector.sortBy(normalized)
    }
    
    def escapeIdentifierIfRequired(identifier: String): String = {
      if (escapingDelimiter.isEmpty) return identifier
      val decomposed: Vector[Int] = Util.decomposeString(identifier)
      def needsEscaping: Boolean = {
        var pos = 0
        val lookahead: Lookahead[Int] = ahead => decomposed.drop(pos + ahead).headOption
        while (true) calc.lexer.lookup(lookahead) match {
          case CharacterMapping(code, _) if pos == 0 && code == CatCode.Digit => return true
          case CharacterMapping(code, _) if !Identifier.contains(code) => return true
          case CharacterMapping(_, content) => pos += content.size
          case TokResult.Eof => return false;
          case TokResult.Ambiguity(_) => pos += 1;
        }
        false
      }
      
      if (needsEscaping) {
        escapingDelimiter.get + escapingTranslator.translate(identifier) + escapingDelimiter.get
      } else {
        identifier
      }
    }
    
    // Calculator properties
    findPrefix(Identifier) match {
      case Some(Prefix(prefix, completionString, matchString)) if prefix.strip() == "set" =>
        return Result(prefix, completionString, findMatches(CalculatorProperties.allProperties, matchString), isIdentifier = false)
      case _ =>
    }
    
    // Catcodes
    findPrefix(SpacedIdentifier, innerCatcodes = JustSpace) match {
      case Some(Prefix(prefix, completionString, matchString)) if (prefix.strip().startsWith("cat ") || prefix.strip().startsWith("tok ")) && calc.lexer.catCode(prefix.strip().codePoints().toArray.last) == CatCode.Assign =>
        return Result(prefix, completionString, findMatches(CatCode.values.map(_.toString), matchString), isIdentifier = false)
      case _ =>
    }
    
    findPrefix(Identifier, startsWith = CatCode.Special) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        return Result(prefix, completionString, findMatches(calc.specials.keys, matchString), isIdentifier = false)
      case None =>
    }
    
    findPrefix(Reference, startsWith = CatCode.Reference) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        return Result(prefix, completionString, findMatches(calc.resolution.tabCompleteReference, matchString), isIdentifier = false)
      case None =>
    }
    
    findPrefix(Identifier) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        val baseMatches: Vector[String] = findMatches(calc.resolution.tabCompleteIdentifier, matchString).map(escapeIdentifierIfRequired)
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
