package tuxcalculator.core.util

import tuxcalculator.api.TuxCalculator.{HighlightType, InputHighlight}
import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorCommands
import tuxcalculator.core.lexer.CatCode.CatCode
import tuxcalculator.core.lexer.{CatCode, CharacterMapping, Lookahead, TokResult}

import scala.collection.mutable.ListBuffer

object InputHighlighter {
  
  private val Number: Set[CatCode] = Set(CatCode.Digit, CatCode.DecimalSep, CatCode.Exp)
  private val Identifier: Set[CatCode] = Set(CatCode.Letter, CatCode.Digit, CatCode.Exp)
  
  def highlight(calc: Calculator, line: String): Vector[InputHighlight] = {
    var idx: Int = 0
    val codePoints = Util.decomposeString(line)
    val parts: ListBuffer[InputHighlight] = ListBuffer()
    
    def advance(amount: Int, highlight: HighlightType): Unit = if (amount != 0) {
      val content = codePoints.slice(idx min codePoints.length, (idx + amount) min codePoints.length)
      idx = (idx + amount) min codePoints.length
      if (content.nonEmpty) parts.lastOption match {
        case Some(hl) if hl.`type`() == highlight =>
          parts.remove(parts.length - 1)
          parts.addOne(new InputHighlight(highlight, hl.content() + Util.makeString(content)))
        case _ => parts.addOne(new InputHighlight(highlight, Util.makeString(content)))
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
    CalculatorCommands.commands(calc).map(Util.decomposeString).find((cmd: Vector[Int]) => codePoints.drop(idx).startsWith(cmd)) match {
      case Some(cmd) => advance(cmd.length, HighlightType.COMMAND)
      case None =>
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
          case CatCode.Operator | CatCode.Sign | CatCode.Post => advance(content.length, HighlightType.OPERATOR)
          case CatCode.Reference =>
            advance(content.length, HighlightType.REFERENCE) // Also updates the lookahead
            skipSpace()
            calc.lexer.lookup(lookahead) match {
              // Signs only take the first token
              case CharacterMapping(CatCode.Sign, nestedContent) => advance(nestedContent.length, HighlightType.REFERENCE)
              case CharacterMapping(CatCode.Operator, _) => advanceWhile(lookahead, HighlightType.REFERENCE, cat => cat.contains(CatCode.Operator))
              case CharacterMapping(CatCode.Post, _) => advanceWhile(lookahead, HighlightType.REFERENCE, cat => cat.contains(CatCode.Post))
              case _ => advanceWhile(lookahead, HighlightType.REFERENCE, cat => cat.exists(Identifier.contains))
            }
          case CatCode.Special =>
            advance(content.length, HighlightType.SPECIAL) // Also updates the lookahead
            skipSpace()
            advanceWhile(lookahead, HighlightType.SPECIAL, cat => cat.exists(Identifier.contains))
          case cat if Number.contains(cat) => advanceWhile(lookahead, HighlightType.NUMBER, cat => cat.exists(Number.contains))
          case cat if Identifier.contains(cat) => advanceWhile(lookahead, HighlightType.IDENTIFIER, cat => cat.exists(Identifier.contains))
          case _ => advance(content.length, HighlightType.PLAIN)
        }
        case _ => advance(1, HighlightType.PLAIN)
      }
    }
    
    parts.toVector
  }
}