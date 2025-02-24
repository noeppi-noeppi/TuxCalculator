package tuxcalculator.core.util

import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.translate.{AggregateTranslator, LookupTranslator}
import tuxcalculator.core.Calculator
import tuxcalculator.core.data.{CalculatorCommands, CalculatorProperties}
import tuxcalculator.core.lexer._

import java.text.Normalizer
import java.util.Locale
import scala.annotation.tailrec
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

    def findCommandEnd(text: String, command: Option[calc.commands.Command], subCommands: List[calc.commands.SubCommand]): Option[Int] = command match {
      case None => Some(0)
      case Some(cmd) => text match {
        case `cmd`(remaining) => findSubCommandEnd(remaining, subCommands)
        case _ => None
      }
    }

    @tailrec
    def findSubCommandEnd(text: RemainingText, subCommands: List[calc.commands.SubCommand]): Option[Int] = subCommands match {
      case Nil if text.string.isEmpty => None // We don't start with a command if the input ends directly after it.
      case Nil =>
        val hasSpaceAfterCommand: Boolean = Util.decomposeString(text.string).takeWhile(codePoint => calc.lexer.catCode(codePoint) == CatCode.Space).nonEmpty
        val commandOffset = if (hasSpaceAfterCommand) text.offset + 1 else text.offset
        Some(commandOffset)
      case cmd :: tail => text match {
        case `cmd`(remaining) => findSubCommandEnd(remaining, tail)
        case _ => None
      }
    }

    def findPrefix(catcodes: Set[CatCode], startsWith: CatCode = null, innerCatcodes: Set[CatCode] = Set(), command: calc.commands.Command = null, subCommands: List[calc.commands.SubCommand] = List(), allowNonCommandPrefix: Boolean = true): Option[Prefix] = {
      val commandEnd: Int = findCommandEnd(line, Option(command), subCommands) match {
        case Some(idx) => idx
        case None => return None
      }

      val rawMatchIdx: Int = codePoints.lastIndexWhere(codePoint => !catcodes.contains(calc.lexer.catCode(codePoint))) match {
        case -1 => -1
        // Inner catcodes may not on the beginning (on the end they are fine though).
        // Treat inner catcodes at the start of the match string as not belonging to the match string
        case outerMatchIdx => codePoints.indexWhere(codePoint => !innerCatcodes.contains(calc.lexer.catCode(codePoint)), outerMatchIdx + 1) match {
          case -1 => codePoints.length - 1 // If all catcodes are inner catcodes, behave as if we have an empty match string
          case idx => idx - 1
        }
      }

      // Never go back before the start of commandEnd.
      val matchIdx: Int = rawMatchIdx `max` (commandEnd - 1)

      val commandEndWithSpacesSkipped: Int = commandEnd + codePoints.drop(commandEnd).takeWhile(codePoint => calc.lexer.catCode(codePoint) == CatCode.Space).length
      if (!allowNonCommandPrefix && matchIdx > commandEndWithSpacesSkipped) return None

      val spaceSkipped = codePoints.lastIndexWhere(codePoint => calc.lexer.catCode(codePoint) != CatCode.Space, matchIdx)
      Option(startsWith) match {
        case Some(startCode) if spaceSkipped < 0 || calc.lexer.catCode(codePoints(spaceSkipped)) != startCode => None
        case startOption =>
          // If there is a start cat code or we follow a tight cat code, skip the entire space.
          // Otherwise keep a single space if present
          val spaceToSkipNoCommand = if ((matchIdx - spaceSkipped) <= 0 || startOption.isDefined || TightCatCodes.contains(calc.lexer.catCode(codePoints(spaceSkipped)))) spaceSkipped else spaceSkipped + 1
          // Make sure we don't go back before commandEnd in space skipping
          val spaceToSkip = spaceToSkipNoCommand `max` (commandEnd - 1)
          Some(Prefix(Util.makeString(codePoints.take(spaceToSkip + 1)), Util.makeString(codePoints.drop(spaceToSkip + 1)), Util.makeString(codePoints.drop(matchIdx + 1))))
      }
    }

    def findMatches(matchString: String, regular: Set[String], priority: Set[String] = Set()): Vector[String] = {
      def normalized(string: String): String = Normalizer.normalize(string, Normalizer.Form.NFKD)
          .replaceAll("\\p{M}", "").toLowerCase(Locale.ROOT)
      val normalizedMatch = normalized(matchString)
      val matchingRegular = regular.filter(str => normalized(str).startsWith(normalizedMatch)).toVector.sortBy(normalized)
      val matchingPriority = priority.filter(str => normalized(str).startsWith(normalizedMatch)).toVector.sortBy(normalized)
      matchingPriority ++ matchingRegular
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
        }
        false
      }

      if (needsEscaping) {
        escapingDelimiter.get + escapingTranslator.translate(identifier) + escapingDelimiter.get
      } else {
        identifier
      }
    }

    // Format codes
    findPrefix(SpacedIdentifier, command = calc.commands.Set, subCommands = calc.commands.Fmt :: Nil, allowNonCommandPrefix = false) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        return Result(prefix, completionString, findMatches(matchString, FmtCode.values.map(_.toString)), isIdentifier = false)
      case _ =>
    }

    // Calculator properties
    findPrefix(Identifier, command = calc.commands.Set, allowNonCommandPrefix = false) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        return Result(prefix, completionString, findMatches(matchString, CalculatorProperties.allProperties, priority = Set(calc.commands.Fmt.name)), isIdentifier = false)
      case _ =>
    }

    // Catcodes
    findPrefix(SpacedIdentifier, innerCatcodes = JustSpace, command = calc.commands.Cat).orElse(findPrefix(SpacedIdentifier, innerCatcodes = JustSpace, command = calc.commands.Tok)) match {
      case Some(Prefix(prefix, completionString, matchString)) if calc.lexer.catCode(prefix.strip().codePoints().toArray.last) == CatCode.Assign =>
        return Result(prefix, completionString, findMatches(matchString, CatCode.values.map(_.toString)), isIdentifier = false)
      case _ =>
    }

    findPrefix(Identifier, startsWith = CatCode.Special) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        return Result(prefix, completionString, findMatches(matchString, calc.specials.keys).map(escapeIdentifierIfRequired), isIdentifier = false)
      case None =>
    }

    findPrefix(Reference, startsWith = CatCode.Reference) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        val operatorMatches = findMatches(matchString, calc.resolution.tabCompleteReferenceOperator)
        val functionMatches = findMatches(matchString, calc.resolution.tabCompleteReferenceFunction).map(escapeIdentifierIfRequired)
        return Result(prefix, completionString, operatorMatches ++ functionMatches, isIdentifier = false)
      case None =>
    }

    findPrefix(Identifier) match {
      case Some(Prefix(prefix, completionString, matchString)) =>
        val baseMatches: Vector[String] = findMatches(matchString, calc.resolution.tabCompleteIdentifier).map(escapeIdentifierIfRequired)
        val matches: Vector[String] = if (prefix.isEmpty) {
          val commands: Set[String] = CalculatorCommands.commands(calc)
          val commandMatches: Vector[String] = findMatches(matchString, commands)
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
