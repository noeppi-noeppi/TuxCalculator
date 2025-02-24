package tuxcalculator.core.lexer

import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.translate.{AggregateTranslator, LookupTranslator}
import tuxcalculator.core.util.{Result, Util}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

case class TokenStream(tokens: Vector[ContextualToken]) {
  override def toString: String = "TokenStream(" + tokens.mkString(",") + ")"
}

case class ContextualToken(token: Token, context: String)

case class RemainingText(string: String, offset: Int)
case class SplitText(before: RemainingText, after: RemainingText)
case class PartialTokenStream(tokens: TokenStream, remaining: RemainingText)

class CharacterSource(private val codePoints: Vector[Int], private[this] val offset: Int = 0) extends Lookahead[Int] {
  private[this] var position: Int = 0
  
  def advance(amount: Int): Unit = position += amount
  def consumed: RemainingText = RemainingText(Util.makeString(codePoints.take(position)), offset)
  def remaining: RemainingText = RemainingText(Util.makeString(codePoints.drop(position)), position + offset)
  def context: String = "At: " + Util.makeString(codePoints.take(position).takeRight(8)).strip() + " <== here: column " + (position + offset)
  override def lookupToken(ahead: Int): Option[Int] = position + ahead match {
    case idx if codePoints.indices.contains(idx) => Some(codePoints(idx))
    case _ => None
  }
}

object Lexer {
  private[this] val UNESCAPE = new AggregateTranslator(
    new LookupTranslator(Map[CharSequence, CharSequence]("\\s" -> " ").asJava),
    StringEscapeUtils.UNESCAPE_JAVA
  )
  
  private val LambdaTerminators: Set[CatCode] = Set(
    CatCode.Close, CatCode.End, CatCode.EndMatch, CatCode.ElementSep, CatCode.GroupSep, CatCode.VarArg, CatCode.Follow
  )
  
  private def safeUnescape(string: String): Result[String] = try {
    Result.Value(UNESCAPE.translate(string))
  } catch {
    case e: IllegalArgumentException => Result.Error(e.getMessage)
  }
}

class Lexer {

  private[this] val catCodes: CatCodes = new CatCodes
  private[this] val fmtCodes: FmtCodes = new FmtCodes

  def catCode(codePoint: Int): CatCode = this.catCodes.catCode(codePoint)
  def catCode(codePoint: Int, code: CatCode): Unit = this.catCodes.catCode(codePoint, code)
  def tokCode(token: String, code: CatCode): Unit = this.catCodes.tokCode(Util.decomposeString(token), code)
  def fmtCode(code: FmtCode, format: String): Unit = this.fmtCodes.fmtCode(code, format)
  def escapeCodePoints: Set[Int] = this.catCodes.escapeCodePoints
  def allChangedCatCodes: Map[Int, CatCode] = this.catCodes.allChangedCatCodes
  def allChangedTokCodes: Map[String, CatCode] = this.catCodes.allChangedTokCodes.map(entry => (Util.makeString(entry._1), entry._2))
  def allChangedFmtCodes: Map[FmtCode, String] = this.fmtCodes.allChangedFmtCodes
  
  def lookup(source: Lookahead[Int]): TokResult = this.catCodes.tokCode(source)
  def format(code: FmtCode): String = this.fmtCodes.fmtCode(code)
  def escapeErrorLiteral(msg: String): String = this.fmtCodes.escError(msg)
  
  def tokenize(line: String): Result[TokenStream] = continue(RemainingText(line, 0))
  def continue(remaining: RemainingText): Result[TokenStream] = tryTokenize {
    val source: CharacterSource = new CharacterSource(Util.decomposeString(remaining.string), remaining.offset)
    tokenizePart(source, Set(), Set()) ~@ source.context ~ (_.tokens)
  }
  
  def tokenizeAssignment(assignmentText: RemainingText): Result[PartialTokenStream] = tryTokenize {
    val source: CharacterSource = new CharacterSource(Util.decomposeString(assignmentText.string), assignmentText.offset)
    val result = tokenizePart(source, Set(), Set(), tokenizeAssignment = true)
    result ~@ source.context ~ (_.tokens) ~ (tokens => PartialTokenStream(tokens, source.remaining))
  }
  
  def splitAssignment(assignmentText: RemainingText): Result[SplitText] = tryTokenize {
    val decomposed = Util.decomposeString(assignmentText.string)
    val source: CharacterSource = new CharacterSource(decomposed, assignmentText.offset)
    val result = tokenizePart(source, Set(), Set(), tokenizeAssignment = true)
    result ~@ source.context ~ (result => {
      val before: RemainingText = source.consumed match {
        case RemainingText(text, offset) => RemainingText(text.dropRight(result.assignToken.getOrElse("").length), offset)
      }
      val after: RemainingText = source.remaining
      SplitText(before, after)
    })
  }
  
  private def tryTokenize[T](action: => Result[T]): Result[T] = try {
    action
  } catch {
    case e: ImmediateError => e.error
  }
  
  private case class PartTokenizeResult(tokens: TokenStream, closingToken: Option[String], assignToken: Option[String])
  
  @throws[ImmediateError]
  private def tokenizePart(source: CharacterSource, closingCatCodes: Set[CatCode], breakAt: Set[CatCode], tokenizeAssignment: Boolean = false, canEmitFollow: Boolean = false): Result[PartTokenizeResult] = {
    val tokens: ListBuffer[ContextualToken] = ListBuffer()
    
    object CatCodeGrouper {
      private[this] val identifier: ListBuffer[Int] = ListBuffer()

      private[this] var insideNumber: Boolean = false
      private[this] var insideFractional: Boolean = false // Must only be true if an actual decimal sep was encountered. Integral part + exp only must leave this false
      private[this] var insideExponent: Boolean = false
      private[this] val integralNumber: ListBuffer[Int] = ListBuffer()
      private[this] val fractionalNumber: ListBuffer[Int] = ListBuffer()
      private[this] val numberExponent: ListBuffer[Int] = ListBuffer()

      private[this] val operator: ListBuffer[Int] = ListBuffer()
      private[this] val postfixOperator: ListBuffer[Int] = ListBuffer()
      
      def isGroupingOperator: Boolean = operator.nonEmpty
      
      def consume(code: CatCode, content: Vector[Int]): Option[Result[Nothing]] = {
        if (code == CatCode.Sign) {
          // Signs (only + and - with sign catcode) directly after an exp catcode in a number contribute to the exponent
          if (insideExponent && numberExponent.isEmpty && content.length == 1 && (content.head == 43 || content.head == 45)) {
            numberExponent.addAll(content)
          } else {
            // Special case: signs are always single operators, so finish everything else
            finish()
            lastTokenOption match {
              case Some(_: Token.Identifier) | Some(_: Token.Number) | Some(_: Token.Post) | Some(_: Token.Error)
                   | Some(_: Token.Group) | Some(_: Token.Application) | Some(_: Token.PrimaryBracket)
                   | Some(_: Token.SecondaryBracket) | Some(_: Token.TertiaryBracket) | Some(_: Token.Match)
                   | Some(_: Token.Lambda) | Some(Token.Answer) =>
                tokens.addOne(Token.Operator(Util.makeString(content)) >> source.context)
              case Some(_: Token.Sign) | Some(_: Token.Operator) | Some(_: Token.Post) if tokens.init.lastOption.map(_.token).contains(Token.Reference) =>
                tokens.addOne(Token.Operator(Util.makeString(content)) >> source.context)
              case _ =>
                tokens.addOne(Token.Sign(Util.makeString(content)) >> source.context)
            }
          }
          return None
        }
        code match {
          case CatCode.Digit if insideExponent => numberExponent.addAll(content)
          case CatCode.Digit if insideFractional => fractionalNumber.addAll(content)
          case CatCode.Digit if insideNumber => integralNumber.addAll(content)
          case CatCode.Letter | CatCode.Exp | CatCode.Digit if identifier.nonEmpty => identifier.addAll(content)
          case CatCode.Operator | CatCode.Assign if operator.nonEmpty => operator.addAll(content)
          case CatCode.Post if postfixOperator.nonEmpty => postfixOperator.addAll(content)
          // Check for cat-codes that appear inside a number
          case CatCode.DecimalSep if !insideNumber || (!insideFractional && !insideExponent) =>
            finishIdentifier()
            finishOperator()
            finishPost()
            insideNumber = true
            insideFractional = true
          case CatCode.DecimalSep => return Some(Result.Error("Dangling decimal separator"))
          case CatCode.Exp if insideNumber && !insideExponent => insideExponent = true
          // Start a new cat-code group
          case CatCode.Digit =>
            finishIdentifier()
            finishOperator()
            finishPost()
            insideNumber = true
            integralNumber.addAll(content)
          case CatCode.Letter | CatCode.Exp =>
            finishNumber()
            finishOperator()
            finishPost()
            identifier.addAll(content)
          case CatCode.Operator | CatCode.Assign =>
            finishIdentifier()
            finishNumber()
            finishPost()
            operator.addAll(content)
          case CatCode.Post =>
            finishIdentifier()
            finishNumber()
            finishPost()
            postfixOperator.addAll(content)
          case _ => return Some(Result.Error("Invalid catcode configuration"))
        }
        None
      }
      
      def finish(): Unit = {
        finishIdentifier()
        finishNumber()
        finishOperator()
        finishPost()
      }
      
      def finishIdentifier(): Unit = if (identifier.nonEmpty) {
        tokens.addOne(Token.Identifier(Util.makeString(identifier)) >> source.context)
        identifier.clear()
      }
      
      def finishNumber(): Unit = if (insideNumber) {
        if (insideFractional && fractionalNumber.isEmpty) {
          // We got a decimal point but no fractional part. This is an error.
          throw new ImmediateError(Result.Error("Dangling decimal separator"))
        }
        if (insideExponent && numberExponent.isEmpty) {
          // We got an exponent separator but no exponent. This is an error.
          throw new ImmediateError(Result.Error("Dangling exponent separator"))
        }
        if (insideExponent && numberExponent.size == 1 && (numberExponent.head == 43 || numberExponent.head == 45)) {
          // We got an exponent sign but no exponent. This is an error.
          throw new ImmediateError(Result.Error("Dangling exponent sign"))
        }
        tokens.addOne(Token.Number(
          if (integralNumber.isEmpty) "0" else Util.makeString(integralNumber),
          if (fractionalNumber.isEmpty) None else Some(Util.makeString(fractionalNumber)),
          if (numberExponent.isEmpty) None else Some(Util.makeString(numberExponent))
        ) >> source.context)
        integralNumber.clear()
        fractionalNumber.clear()
        numberExponent.clear()
        insideNumber = false
        insideFractional = false
        insideExponent = false
      }
      
      def finishOperator(): Unit = if (operator.nonEmpty) {
        tokens.addOne(Token.Operator(Util.makeString(operator)) >> source.context)
        operator.clear()
      }
      
      def finishPost(): Unit = if (postfixOperator.nonEmpty) {
        tokens.addOne(Token.Post(Util.makeString(postfixOperator)) >> source.context)
        postfixOperator.clear()
      }
    }

    def emit(token: Token): Unit = {
      CatCodeGrouper.finish()
      tokens.addOne(token >> source.context)
    }
    def finish(closingToken: Option[String] = None, assignToken: Option[String] = None): Result[PartTokenizeResult] = {
      CatCodeGrouper.finish()
      if (tokenizeAssignment && assignToken.isEmpty) {
        Result.Error("Input ended prematurely, expected an assignment token.")
      } else if (!tokenizeAssignment && assignToken.isDefined) {
        Result.Error("Invalid catcode configuration.")
      } else {
        Result.Value(PartTokenizeResult(TokenStream(tokens.toVector), closingToken, assignToken))
      }
    }
    def lastTokenOption: Option[Token] = tokens.lastOption.map(_.token)

    while (true) this.catCodes.tokCode(source) match {
      case TokResult.Eof if closingCatCodes.isEmpty => return finish(None)
      case TokResult.Eof if closingCatCodes.sizeIs == 1 => return Result.Error("Closed expression is not terminated. Expected " + closingCatCodes.head)
      case TokResult.Eof => return Result.Error("Closed expression is not terminated. Expected one of " + closingCatCodes.mkString(", "))
      case CharacterMapping(code, content) => 
        // If there is the possibility to break before a lookahead catcode, we must always break when a comment starts
        // or we might actually read after the comment.
        if (breakAt.contains(code) || (breakAt.nonEmpty && code == CatCode.Comment)) return finish(None)
        source.advance(content.length)
        code match {
          case _ if closingCatCodes.contains(code) => return finish(closingToken = Some(Util.makeString(content)))
          case CatCode.Assign if tokenizeAssignment && !CatCodeGrouper.isGroupingOperator => return finish(None, assignToken = Some(Util.makeString(content)))
          case CatCode.Letter | CatCode.Digit | CatCode.DecimalSep | CatCode.Exp | CatCode.Operator | CatCode.Assign | CatCode.Sign | CatCode.Post => CatCodeGrouper.consume(code, content) match {
            case Some(result) => return result
            case None =>
          }
          case CatCode.Space => CatCodeGrouper.finish()
          case CatCode.ElementSep => emit(Token.ElementSep)
          case CatCode.GroupSep => emit(Token.GroupSep)
          case CatCode.Reference => emit(Token.Reference)
          case CatCode.Error => this.tokenizeInterpolatedStringUntil(source, CatCode.Error) match {
            case Left(token) => emit(token)
            case Right(result) => return result;
          }
          case CatCode.Open =>
            CatCodeGrouper.finish()
            lastTokenOption match {
              case Some(_: Token.Identifier) | Some(_: Token.Number) | Some(_: Token.Post) | Some(_: Token.Application)
                   | Some(_: Token.Group) | Some(_: Token.Error) | Some(_: Token.PrimaryBracket) | Some(_: Token.SecondaryBracket)
                   | Some(_: Token.TertiaryBracket) | Some(_: Token.Match) | Some(_: Token.Lambda) | Some(Token.PartialApplication)
                   | Some(Token.Answer) =>
                this.tokenizePart(source, Set(CatCode.Close), Set()) match {
                  case Result.Value(PartTokenizeResult(tokens: TokenStream, _, _)) => emit(Token.Application(tokens))
                  case result => return result
                }
              // References to operators can be applied
              case Some(_: Token.Operator) | Some(_: Token.Sign) | Some(_: Token.Post) if tokens.length >= 2 && tokens(tokens.length - 2).token == Token.Reference =>
                this.tokenizePart(source, Set(CatCode.Close), Set()) match {
                  case Result.Value(PartTokenizeResult(tokens: TokenStream, _, _)) => emit(Token.Application(tokens))
                  case result => return result
                }
              case _ =>
                this.tokenizePart(source, Set(CatCode.Close), Set()) match {
                  case Result.Value(PartTokenizeResult(tokens: TokenStream, _, _)) => emit(Token.Group(tokens))
                  case result => return result
                }
            }
          case CatCode.StartPrimary => this.tokenizePart(source, Set(CatCode.End, CatCode.EndMatch), Set()) match {
            case Result.Value(PartTokenizeResult(bracketTokens: TokenStream, _, _)) if bracketTokens.tokens.nonEmpty && lastTokenOption.contains(Token.Reference) => return Result.Error("Bracket reference can't contain elements.")
            case Result.Value(PartTokenizeResult(bracketTokens: TokenStream, Some(closingToken: String), _)) => emit(Token.PrimaryBracket(Util.makeString(content), closingToken, bracketTokens))
            case Result.Value(_) => return Result.Error("Could not tokenize input: Closed expression was gobbled without an end token when parsing primary. This should not happen.")
            case result => return result
          }
          case CatCode.StartSecondary => this.tokenizePart(source, Set(CatCode.End, CatCode.EndMatch), Set()) match {
            case Result.Value(PartTokenizeResult(bracketTokens: TokenStream, _, _)) if bracketTokens.tokens.nonEmpty && lastTokenOption.contains(Token.Reference) => return Result.Error("Bracket reference can't contain elements.")
            case Result.Value(PartTokenizeResult(bracketTokens: TokenStream, Some(closingToken: String), _)) => emit(Token.SecondaryBracket(Util.makeString(content), closingToken, bracketTokens))
            case Result.Value(_) => return Result.Error("Could not tokenize input: Closed expression was gobbled without an end token when parsing secondary. This should not happen.")
            case result => return result
          }
          case CatCode.StartTertiary => this.tokenizePart(source, Set(CatCode.End, CatCode.EndMatch), Set()) match {
            case Result.Value(PartTokenizeResult(bracketTokens: TokenStream, _, _)) if bracketTokens.tokens.nonEmpty && lastTokenOption.contains(Token.Reference) => return Result.Error("Bracket reference can't contain elements.")
            case Result.Value(PartTokenizeResult(bracketTokens: TokenStream, Some(closingToken: String), _)) => emit(Token.TertiaryBracket(Util.makeString(content), closingToken, bracketTokens))
            case Result.Value(_) => return Result.Error("Could not tokenize input: Closed expression was gobbled without an end token when parsing tertiary. This should not happen.")
            case result => return result
          }
          case CatCode.StartMatch => this.tokenizePart(source, Set(CatCode.EndMatch), Set(), canEmitFollow = true) match {
            case Result.Value(PartTokenizeResult(matchTokens: TokenStream, _, _)) => emit(Token.Match(matchTokens))
            case result => return result
          }
          case CatCode.Close | CatCode.End | CatCode.EndMatch if closingCatCodes.isEmpty => return Result.Error("Dangling token: " + code)
          case CatCode.Close | CatCode.End | CatCode.EndMatch => return Result.Error("Wrong closing token, expected one of " + closingCatCodes.mkString(", ") + ", got " + code)
          case CatCode.Lambda => this.tokenizePart(source, Set(CatCode.Follow), Set()) match {
            case Result.Value(PartTokenizeResult(argumentTokens: TokenStream, _, _)) => this.tokenizePart(source, Set(), Lexer.LambdaTerminators) match {
              case Result.Value(PartTokenizeResult(definitionTokens: TokenStream, _, _)) => emit(Token.Lambda(argumentTokens, definitionTokens))
              case result => return result
            }
            case result => return result
          }
          case CatCode.Follow if canEmitFollow => emit(Token.Follow)
          case CatCode.Follow => return Result.Error("Dangling token: " + CatCode.Follow + " (missing lambda?)")
          case CatCode.Escape => this.tokenizeStringUntil(source, CatCode.Escape) match {
            case Left(string) =>
              CatCodeGrouper.finish()
              CatCodeGrouper.consume(CatCode.Letter, Util.decomposeString(string))
              CatCodeGrouper.finish()
            case Right(result) => return result;
          }
          case CatCode.Special => emit(Token.Special)
          case CatCode.Guard => emit(Token.Guard)
          case CatCode.Comment if closingCatCodes.isEmpty => return finish(None)
          case CatCode.Comment => return Result.Error("Comment inside closed expression")
          case CatCode.VarArg => emit(Token.Vararg)
          case CatCode.Partial => emit(Token.PartialApplication)
          case CatCode.Answer => emit(Token.Answer)
          case CatCode.Interpolate => return Result.Error("Interpolation is not possible here.")
          case CatCode.Invalid => content.headOption match {
            case Some(char) => return Result.Error("Invalid character: " + Character.getName(char))
            case None => return Result.Error("Invalid tok-code")
          }
        }
    }
    throw new Error()
  }
  
  private def tokenizeStringUntil(source: CharacterSource, code: CatCode): Either[String, Result[Nothing]] = tokenizeStringWithoutEscaping(source, code) match {
    case Left(string) => Lexer.safeUnescape(string).either
    case Right(result) => Right(result)
  }
  
  private def tokenizeInterpolatedStringUntil(source: CharacterSource, code: CatCode): Either[Token.Error, Result[Nothing]] = tokenizeStringWithoutEscaping(source, code) match {
    case Left(string) =>
      val source: CharacterSource = new CharacterSource(Util.decomposeString(string))
      val parts: ListBuffer[Token.Error.TailPart] = ListBuffer()
      var interpolatePrefix: String = ""
      var interpolateName: String = ""
      val sb: StringBuilder = new StringBuilder()
      while (true) source.lookupToken(0) match {
        case None => Lexer.safeUnescape(sb.toString()) match {
          case Result.Value(unescaped) =>
            parts.addOne(Token.Error.TailPart(interpolatePrefix, interpolateName, unescaped))
            return Lexer.safeUnescape(string)
              .map(fullUnescaped => Token.Error(fullUnescaped, parts.head.followingText, parts.tail.toVector))
              .either
          case err: Result.Error => return Right(err)
        }
        case Some(currentChar) if sb.nonEmpty && sb.last == '\\' && (sb.toString.reverseIterator.takeWhile(_ == '\\').length % 2) != 0 =>
          sb.append(Character.toString(currentChar))
          source.advance(1)
        case Some(currentChar) => this.catCodes.tokCode(source) match {
          case CharacterMapping(CatCode.Interpolate, interpolationSignContent) =>
            source.advance(interpolationSignContent.length)
            this.catCodes.tokCode(source) match {
              case CharacterMapping(code@(CatCode.Letter | CatCode.Exp | CatCode.Escape), _) => Lexer.safeUnescape(sb.toString()) match {
                case Result.Value(unescaped) =>
                  parts.addOne(Token.Error.TailPart(interpolatePrefix, interpolateName, unescaped))
                  sb.clear()
                  interpolatePrefix = Util.makeString(interpolationSignContent)
                  interpolateName = consumeIdentifierFromSource(source) match {
                    case Left(identifier) => identifier
                    case Right(result) => return Right(result)
                  }
                  // Only skip space if the identifier isn't escaped
                  if (code != CatCode.Escape) {
                    this.catCodes.tokCode(source) match {
                      case CharacterMapping(CatCode.Space, spaceContent) =>
                        source.advance(spaceContent.length)
                      case _ =>
                    }
                  }
                case err: Result.Error => return Right(err)
              }
              case _ => sb.append(Util.makeString(interpolationSignContent))
            }
          case _ =>
            sb.append(Character.toString(currentChar))
            source.advance(1)
        }
      }
      throw new Error()
    case Right(result) => Right(result)
  }
  
  private def tokenizeStringWithoutEscaping(source: CharacterSource, code: CatCode): Either[String, Result[Nothing]] = {
    val sb = new StringBuilder()
    while (true) source.lookupToken(0) match {
      case None => return Right(Result.Error("Unclosed escape: expected " + code))
      case Some(currentChar) if sb.nonEmpty && sb.last == '\\' && (sb.toString.reverseIterator.takeWhile(_ == '\\').length % 2) != 0 =>
        sb.append(Character.toString(currentChar))
        source.advance(1)
      case Some(currentChar) => this.catCodes.tokCode(source) match {
        case CharacterMapping(matchCode, content) if code == matchCode =>
          source.advance(content.length)
          return Left(sb.toString())
        case _ =>
          sb.append(Character.toString(currentChar))
          source.advance(1)
      }
    }
    throw new Error()
  }
  
  private def consumeIdentifierFromSource(source: CharacterSource): Either[String, Result[Nothing]] = this.catCodes.tokCode(source) match {
    case CharacterMapping(CatCode.Escape, openingEscape) =>
      source.advance(openingEscape.length)
      tokenizeStringUntil(source, CatCode.Escape)
    case _ => 
      val sb: StringBuilder = new StringBuilder()
      while (true) this.catCodes.tokCode(source) match {
        case CharacterMapping(code, content) if code == CatCode.Letter || code == CatCode.Digit || code == CatCode.Exp =>
          source.advance(content.length)
          sb.append(Util.makeString(content))
        case _ => return Left(sb.toString)
      }
      throw new Error()
  }
  
  private class ImmediateError(val error: Result.Error) extends Exception
}
