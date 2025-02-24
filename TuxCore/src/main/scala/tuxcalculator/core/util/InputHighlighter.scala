package tuxcalculator.core.util

import tuxcalculator.api.TuxCalculator.{HighlightPart, HighlightType}
import tuxcalculator.core.Calculator
import tuxcalculator.core.data.{CalculatorCommands, CalculatorProperties}
import tuxcalculator.core.lexer.{CatCode, CharacterMapping, FmtCode, Lookahead, TokResult}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object InputHighlighter {
  
  private val NumberStart: Set[CatCode] = Set(CatCode.Digit, CatCode.DecimalSep)
  private val Number: Set[CatCode] = NumberStart | Set(CatCode.Exp)
  private val Identifier: Set[CatCode] = Set(CatCode.Letter, CatCode.Digit, CatCode.Exp)
  
  def highlight(calc: Calculator, line: String): Vector[HighlightPart] = {
    if (!calc.properties(CalculatorProperties.Highlight)) return Vector(new HighlightPart(HighlightType.PLAIN, line))
    
    var idx: Int = 0
    val codePoints = Util.decomposeString(line)
    val parts: ListBuffer[HighlightPart] = ListBuffer()
    
    def advanceToEnd(highlight: String => HighlightType): Unit = {
      val content = codePoints.slice(idx min codePoints.length, codePoints.length)
      idx = codePoints.length
      if (content.nonEmpty) {
        val highlightType = highlight(Util.makeString(content))
        parts.lastOption match {
          case Some(hl) if hl.`type`() == highlightType =>
            parts.remove(parts.length - 1)
            parts.addOne(new HighlightPart(highlightType, hl.content() + Util.makeString(content)))
          case _ => parts.addOne(new HighlightPart(highlightType, Util.makeString(content)))
        }
      }
    }
    
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
    def advanceEscaped(lookahead: Lookahead[Int], highlight: HighlightType, breakAt: CatCode*)(inner: PartialFunction[TokResult, Boolean]): Unit = {
      var escape: Boolean = false
      while (calc.lexer.lookup(lookahead) match {
        case TokResult.Eof => false
        case CharacterMapping(_, content) if escape && content.nonEmpty =>
          advance(1, highlight)
          escape = false
          true
        case CharacterMapping(_, content) if content.startsWith("\\") =>
          advance(1, highlight)
          escape = true
          true
        case inner(continue) => continue
        case CharacterMapping(_, content) =>
          advance(content.size, highlight) // Prevent an infinite loop when a breakAt catcode is not matched by inner.
          advanceWhile(lookahead, highlight, cat => !cat.exists(breakAt.contains), consumeBackslash = false)
          true
        case _ =>
          advanceWhile(lookahead, highlight, cat => !cat.exists(breakAt.contains), consumeBackslash = false)
          true
      }) {}
    }
    def advanceWhile(lookahead: Lookahead[Int], highlight: HighlightType, test: Option[CatCode] => Boolean, consumeBackslash: Boolean = true): Unit = {
      var amount = 0
      while(calc.lexer.lookup((ahead: Int) => lookahead.lookupToken(amount + ahead)) match {
        case TokResult.Eof => amount += 1; false // Prevent an infinite loop
        case CharacterMapping(code, _) if !test(Some(code)) => false
        case CharacterMapping(_, content) if !consumeBackslash && content.contains('\\') => amount += content.indexOf('\\'); false
        case CharacterMapping(_, content) => amount += content.length; true
        case _ if test(None) => amount += 1; true
        case _ => false
      }) {}
      advance(amount, highlight)
    }
    def advanceSingleIdentifier(lookahead: Lookahead[Int], highlight: HighlightType, stopAt: CatCode*): Unit = calc.lexer.lookup(lookahead) match {
      case CharacterMapping(CatCode.Escape, content) if !stopAt.contains(CatCode.Escape) =>
        advance(content.length, highlight) // Also updates the lookahead
        advanceEscaped(lookahead, highlight, breakAt = Seq(CatCode.Escape) ++ stopAt: _*) {
          case CharacterMapping(code, _) if stopAt.contains(code) => false
          case CharacterMapping(CatCode.Escape, content) =>
            advance(content.length, highlight)
            false
        }
      case _ => advanceWhile(lookahead, highlight, cat => !cat.exists(stopAt.contains) && cat.exists(Identifier.contains))
    }
    def maybeAdvanceCommand(commands: Set[String]): Option[String] = commands.map(Util.decomposeString).find((cmd: Vector[Int]) => codePoints.drop(idx).startsWith(cmd)) match {
      case Some(cmd) if codePoints.drop(idx + cmd.length).headOption.forall(next => !Identifier.contains(calc.lexer.catCode(next))) =>
        advance(cmd.length, HighlightType.COMMAND)
        Some(Util.makeString(cmd))
      case _ => None
    }
    def advanceUntilNextAssign(lookahead: Lookahead[Int])(typeFunc: PartialFunction[String, HighlightType]): Unit = {
      skipSpace()
      var off = 0
      var offNonSpace = 0
      var done = false
      while (!done) {
        calc.lexer.lookup(lookahead.offset(off)) match {
          case CharacterMapping(CatCode.Assign, _) => done = true
          case CharacterMapping(CatCode.Space, content) => off += content.length
          case CharacterMapping(_, content) => off += content.length; offNonSpace = off
          case _ => done = true
        }
      }
      Util.makeString(codePoints.slice(idx, idx + offNonSpace)) match {
        case typeFunc(highlightType) => advance(offNonSpace, highlightType)
        case _ => advance(offNonSpace, HighlightType.PLAIN)
      }
      skipSpace()
    }
    
    // Lookahead that updates with each advance
    val lookahead: Lookahead[Int] = (ahead: Int) => codePoints.drop(idx).drop(ahead).headOption
    
    skipSpace()
    val initialCommandName: Option[String] = maybeAdvanceCommand(CalculatorCommands.commands(calc))
    skipSpace()
    
    var commandAssign: Boolean = initialCommandName.exists(CalculatorCommands.isAssignmentCommand)

    val commandName = initialCommandName match {
      case Some("set") => maybeAdvanceCommand(Set("fmt")) match {
        case Some("fmt") =>
          advanceUntilNextAssign(lookahead) {
            case fmtCodeString if FmtCode.byName(fmtCodeString).isDefined => HighlightType.CONSTRUCT
          }
          Some("set fmt")
        case _ =>
          advanceUntilNextAssign(lookahead) {
            case property if CalculatorProperties.allProperties.contains(property) => HighlightType.CONSTRUCT
          }
          initialCommandName
      }
      case _ => initialCommandName
    }

    val bracketStack: mutable.Stack[CatCode] = mutable.Stack()
    //noinspection LoopVariableNotUpdated
    while (idx < codePoints.length) {
      skipSpace()
      calc.lexer.lookup(lookahead) match {
        case CharacterMapping(code, content) => code match {
          case CatCode.Comment => advance(codePoints.length, HighlightType.COMMENT)
          case CatCode.Error =>
            advance(content.length, HighlightType.ERROR) // Also updates the lookahead
            advanceEscaped(lookahead, HighlightType.ERROR, breakAt = CatCode.Error, CatCode.Interpolate) {
              case CharacterMapping(CatCode.Error, content) =>
                advance(content.length, HighlightType.ERROR)
                false
              case CharacterMapping(CatCode.Interpolate, content) => calc.lexer.lookup(lookahead.offset(content.length)) match {
                case CharacterMapping(CatCode.Letter | CatCode.Exp | CatCode.Escape, _) =>
                  advance(content.length, HighlightType.CONSTRUCT)
                  advanceSingleIdentifier(lookahead, HighlightType.CONSTRUCT, stopAt = CatCode.Error)
                  true
                case _ =>
                  advance(content.length, HighlightType.ERROR)
                  true
              }
            }
          case CatCode.Assign if commandAssign =>
            advance(content.length, HighlightType.COMMAND)
            commandAssign = false
            if (commandName.contains("cat") || commandName.contains("tok")) {
              advanceToEnd(leftOver => if (CatCode.byName(leftOver.strip()).isDefined) HighlightType.CONSTRUCT else HighlightType.PLAIN)
            }
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
              case _ => advanceSingleIdentifier(lookahead, HighlightType.REFERENCE)
            }
          case CatCode.Special =>
            advance(content.length, HighlightType.SPECIAL) // Also updates the lookahead
            skipSpace()
            advanceSingleIdentifier(lookahead, HighlightType.SPECIAL)
          case cat if NumberStart.contains(cat) => advanceWhile(lookahead, HighlightType.NUMBER, cat => cat.exists(Number.contains))
          case cat if cat == CatCode.Escape || Identifier.contains(cat) => advanceSingleIdentifier(lookahead, HighlightType.IDENTIFIER)
          case CatCode.StartPrimary | CatCode.StartSecondary | CatCode.StartTertiary =>
            bracketStack.push(code)
            advance(content.length, HighlightType.PLAIN)
          case CatCode.StartMatch =>
            bracketStack.push(CatCode.StartMatch)
            advance(content.length, HighlightType.CONSTRUCT)
          case CatCode.End =>
            if (bracketStack.nonEmpty) bracketStack.pop()
            advance(content.length, HighlightType.PLAIN)
          case CatCode.EndMatch =>
            if (bracketStack.nonEmpty && bracketStack.pop() == CatCode.StartMatch) {
              advance(content.length, HighlightType.CONSTRUCT)
            } else {
              advance(content.length, HighlightType.PLAIN)
            }
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
