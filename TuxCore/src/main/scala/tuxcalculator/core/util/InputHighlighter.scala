package tuxcalculator.core.util

import tuxcalculator.api.TuxCalculator.{HighlightType, HighlightPart}
import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorCommands
import tuxcalculator.core.lexer.CatCode.CatCode
import tuxcalculator.core.lexer.{CatCode, CharacterMapping, Lookahead, TokResult}

import scala.collection.mutable.ListBuffer

object InputHighlighter {
  
  private val Number: Set[CatCode] = Set(CatCode.Digit, CatCode.DecimalSep, CatCode.Exp)
  private val Identifier: Set[CatCode] = Set(CatCode.Letter, CatCode.Digit, CatCode.Exp)
  
  def highlight(calc: Calculator, line: String): Vector[HighlightPart] = {
    var idx: Int = 0
    val codePoints = Util.decomposeString(line)
    val parts: ListBuffer[HighlightPart] = ListBuffer()
    
    def advance(amount: Int, highlight: HighlightType): Unit = if (amount != 0) {
      val content = codePoints.slice(idx min codePoints.length, (idx + amount) min codePoints.length)
      idx = (idx + amount) min codePoints.length
      if (content.nonEmpty) parts.lastOption match {
        case Some(hl) if hl.`type`() == highlight =>
          parts.remove(parts.length - 1)
          parts.addOne(new HighlightPart(highlight, hl.content() + Util.makeString(content)))
        case _ => parts.addOne(new HighlightPart(highlight, Util.makeString(content)))
      }
    }
    
    def skipSpace(): Unit = advance(codePoints.drop(idx).takeWhile(cp => calc.lexer.catCode(cp) == CatCode.Space).length, HighlightType.PLAIN)
    def advanceWhile(lookahead: Lookahead[Int], highlight: HighlightType, test: Option[CatCode] => Boolean, takeFirstNonMatch: Boolean = false): Unit = {
      var amount = 0
      val nonMatchFactor = if (takeFirstNonMatch) 1 else 0
      while(calc.lexer.lookup((ahead: Int) => lookahead.lookupToken(amount + ahead)) match {
        case TokResult.Eof => amount += 1; false // Prevent an infinite loop
        case CharacterMapping(code, content) if test(Some(code)) => amount += content.length; true
        case CharacterMapping(_, content) => amount += (content.length * nonMatchFactor); false
        case _ if test(None) => amount += 1; true
        case _  => amount += (1 * nonMatchFactor); false
      }) {}
      advance(amount, highlight)
    }
    
    skipSpace()
    var commandAssign: Boolean = CalculatorCommands.commands(calc).map(Util.decomposeString).find((cmd: Vector[Int]) => codePoints.drop(idx).startsWith(cmd)) match {
      case Some(cmd) if codePoints.drop(idx + cmd.length).headOption.forall(next => !Identifier.contains(calc.lexer.catCode(next))) =>
        advance(cmd.length, HighlightType.COMMAND)
        CalculatorCommands.isAssignmentCommand(Util.makeString(cmd))
      case _ => false
    }

    // Lookahead that updates with each advance
    val lookahead: Lookahead[Int] = (ahead: Int) => codePoints.drop(idx).drop(ahead).headOption
    //noinspection LoopVariableNotUpdated
    while (idx < codePoints.length) {
      skipSpace()
      calc.lexer.lookup(lookahead) match {
        case CharacterMapping(code, content) => code match {
          case CatCode.Comment => advance(codePoints.length, HighlightType.COMMENT)
          case CatCode.Escape =>
            advance(content.length, HighlightType.IDENTIFIER) // Also updates the lookahead
            advanceWhile(lookahead, HighlightType.IDENTIFIER, cat => !cat.contains(CatCode.Escape), takeFirstNonMatch = true)
          case CatCode.Error =>
            advance(content.length, HighlightType.ERROR) // Also updates the lookahead
            advanceWhile(lookahead, HighlightType.ERROR, cat => !cat.contains(CatCode.Error), takeFirstNonMatch = true)
          case CatCode.Assign if commandAssign =>
            advance(content.length, HighlightType.COMMAND)
            commandAssign = false
          case CatCode.Operator | CatCode.Sign | CatCode.Post | CatCode.Assign => advance(content.length, HighlightType.OPERATOR)
          case CatCode.Reference =>
            advance(content.length, HighlightType.REFERENCE) // Also updates the lookahead
            skipSpace()
            calc.lexer.lookup(lookahead) match {
              // Signs only take the first token
              case CharacterMapping(CatCode.Sign, nestedContent) => advance(nestedContent.length, HighlightType.REFERENCE)
              case CharacterMapping(CatCode.Operator | CatCode.Assign, _) => advanceWhile(lookahead, HighlightType.REFERENCE, cat => cat.contains(CatCode.Operator) || cat.contains(CatCode.Assign))
              case CharacterMapping(CatCode.Post, _) => advanceWhile(lookahead, HighlightType.REFERENCE, cat => cat.contains(CatCode.Post))
              case CharacterMapping(CatCode.StartPrimary | CatCode.StartSecondary | CatCode.StartTertiary, nestedContent) =>
                advance(nestedContent.length, HighlightType.REFERENCE)
                calc.lexer.lookup(lookahead) match {
                  case CharacterMapping(CatCode.End | CatCode.EndMatch, endContent) => advance(endContent.length, HighlightType.REFERENCE)
                  case _ =>
                }
              case _ => advanceWhile(lookahead, HighlightType.REFERENCE, cat => cat.exists(Identifier.contains))
            }
          case CatCode.Special =>
            advance(content.length, HighlightType.SPECIAL) // Also updates the lookahead
            skipSpace()
            advanceWhile(lookahead, HighlightType.SPECIAL, cat => cat.exists(Identifier.contains))
          case cat if Number.contains(cat) => advanceWhile(lookahead, HighlightType.NUMBER, cat => cat.exists(Number.contains))
          case cat if Identifier.contains(cat) => advanceWhile(lookahead, HighlightType.IDENTIFIER, cat => cat.exists(Identifier.contains))
          case CatCode.Answer | CatCode.Lambda | CatCode.Follow | CatCode.VarArg | CatCode.Partial => advance(content.length, HighlightType.CONSTRUCT)
          case _ => advance(content.length, HighlightType.PLAIN)
        }
        case _ => advance(1, HighlightType.PLAIN)
      }
    }
    
    // Identifiers are not necessarily processed as a whole but may be added in pieces.
    // At this point however, they'll be merged together.
    // Replace identifiers that are globally defined with global highlight.
    parts.map(part => {
      if (part.`type`() == HighlightType.IDENTIFIER && calc.resolution.tabCompleteIdentifier.contains(part.content())) {
        new HighlightPart(HighlightType.GLOBAL, part.content())
      } else {
        part
      }
    }).toVector
  }
}
