package tuxcalculator.core.lexer

import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.translate.{AggregateTranslator, LookupTranslator}
import tuxcalculator.core.lexer.Lexer.UNESCAPE
import tuxcalculator.core.util.{Result, Util}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

case class TokenStream(tokens: Vector[Token]) {
  override def toString: String = "TokenStream(" + tokens.mkString(",") + ")"
}

case class PartialTokenStream(tokens: TokenStream, remaining: String)

class CharacterSource(private val codePoints: Vector[Int]) extends Lookahead[Int] {
  private var position: Int = 0
  
  def advance(amount: Int): Unit = position += amount
  def remaining: String = Util.makeString(codePoints.drop(position))
  override def lookupToken(ahead: Int): Option[Int] = position + ahead match {
    case idx if codePoints.indices.contains(idx) => Some(codePoints(idx))
    case _ => None
  }
}

object Lexer {
  val UNESCAPE = new AggregateTranslator(
    new LookupTranslator(Map[CharSequence, CharSequence]("\\s" -> " ").asJava),
    StringEscapeUtils.UNESCAPE_JAVA
  )
  
  val LambdaTerminators: Set[CatCode] = Set(
    CatCode.Close, CatCode.End, CatCode.EndMatch, CatCode.ElementSep, CatCode.GroupSep, CatCode.VarArg, CatCode.Follow
  )
}

class Lexer {

  private[this] val catCodes: CatCodes = new CatCodes

  def catCode(codePoint: Int): CatCode = this.catCodes.catCode(codePoint)
  def catCode(codePoint: Int, code: CatCode): Unit = this.catCodes.catCode(codePoint, code)
  def tokCode(token: String, code: CatCode): Unit = this.catCodes.tokCode(Util.decomposeString(token), code)
  def allChangedCatCodes: Map[Int, CatCode] = this.catCodes.allChangedCatCodes
  def allChangedTokCodes: Map[String, CatCode] = this.catCodes.allChangedTokCodes.map(entry => (Util.makeString(entry._1), entry._2))
  
  def lookup(source: Lookahead[Int]): TokResult = this.catCodes.tokCode(source)
  
  def tokenize(line: String): Result[TokenStream] = try {
    val source: CharacterSource = new CharacterSource(Util.decomposeString(line))
    tokenizePart(source, Set(), Set()) ~ (_._1)
  } catch {
    case e: ImmediateError => e.error
  }
  
  def tokenizeAssignment(line: String): Result[PartialTokenStream] = try {
    val source: CharacterSource = new CharacterSource(Util.decomposeString(line))
    val result = tokenizePart(source, Set(), Set(), tokenizeAssignment = true)
    result ~ (_._1) ~ (tokens => PartialTokenStream(tokens, source.remaining))
  } catch {
    case e: ImmediateError => e.error
  }
  
  @throws[ImmediateError]
  private def tokenizePart(source: CharacterSource, closingCatCodes: Set[CatCode], breakAt: Set[CatCode], tokenizeAssignment: Boolean = false, canEmitFollow: Boolean = false): Result[(TokenStream, Option[String])] = {
    val tokens: ListBuffer[Token] = ListBuffer()
    
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
            tokens.lastOption match {
              case Some(_: Token.Identifier) | Some(_: Token.Number) | Some(_: Token.Post) | Some(_: Token.Error)
                   | Some(_: Token.Group) | Some(_: Token.Application) | Some(_: Token.PrimaryBracket)
                   | Some(_: Token.SecondaryBracket) | Some(_: Token.TertiaryBracket) | Some(_: Token.Match)
                   | Some(_: Token.Lambda) | Some(Token.Answer) =>
                tokens.addOne(Token.Operator(Util.makeString(content)))
              case Some(_: Token.Sign) | Some(_: Token.Operator) | Some(_: Token.Post) if tokens.init.lastOption.contains(Token.Reference) =>
                tokens.addOne(Token.Operator(Util.makeString(content)))
              case _ =>
                tokens.addOne(Token.Sign(Util.makeString(content)))
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
        tokens.addOne(Token.Identifier(Util.makeString(identifier)))
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
        ))
        integralNumber.clear()
        fractionalNumber.clear()
        numberExponent.clear()
        insideNumber = false
        insideFractional = false
        insideExponent = false
      }
      
      def finishOperator(): Unit = if (operator.nonEmpty) {
        tokens.addOne(Token.Operator(Util.makeString(operator)))
        operator.clear()
      }
      
      def finishPost(): Unit = if (postfixOperator.nonEmpty) {
        tokens.addOne(Token.Post(Util.makeString(postfixOperator)))
        postfixOperator.clear()
      }
    }

    def emit(token: Token): Unit = {
      CatCodeGrouper.finish()
      tokens.addOne(token)
    }
    def finish(closingToken: Option[String], isAssignToken: Boolean = false): Result[(TokenStream, Option[String])] = {
      CatCodeGrouper.finish()
      if (tokenizeAssignment && !isAssignToken) {
        Result.Error("Input ended prematurely, expected an assignment token.")
      } else if (!tokenizeAssignment && isAssignToken) {
        Result.Error("Invalid catcode configuration.")
      } else {
        Result.Value((TokenStream(tokens.toVector), closingToken))
      }
    }

    while (true) this.catCodes.tokCode(source) match {
      case TokResult.Eof if closingCatCodes.isEmpty => return finish(None)
      case TokResult.Eof if closingCatCodes.sizeIs == 1 => return Result.Error("Closed expression is not terminated. Expected " + closingCatCodes.head)
      case TokResult.Eof => return Result.Error("Closed expression is not terminated. Expected one of " + closingCatCodes.mkString(", "))
      case TokResult.Ambiguity(matches) => return Result.Error("Ambiguous tok-codes: Multiple matches: " + matches.mkString(", "))
      case CharacterMapping(code, content) => 
        // If there is the possibility to break before a lookahead catcode, we must always break when a comment starts
        // or we might actually read after the comment.
        if (breakAt.contains(code) || (breakAt.nonEmpty && code == CatCode.Comment)) return finish(None)
        source.advance(content.length)
        code match {
          case _ if closingCatCodes.contains(code) => return finish(Some(Util.makeString(content)))
          case CatCode.Assign if tokenizeAssignment && !CatCodeGrouper.isGroupingOperator => return finish(None, isAssignToken = true)
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
            tokens.lastOption match {
              case Some(_: Token.Identifier) | Some(_: Token.Number) | Some(_: Token.Post) | Some(_: Token.Application)
                   | Some(_: Token.Group) | Some(_: Token.Error) | Some(_: Token.PrimaryBracket) | Some(_: Token.SecondaryBracket)
                   | Some(_: Token.TertiaryBracket) | Some(_: Token.Match) | Some(_: Token.Lambda) | Some(Token.PartialApplication)
                   | Some(Token.Answer) =>
                this.tokenizePart(source, Set(CatCode.Close), Set()) match {
                  case Result.Value((tokens: TokenStream, _)) => emit(Token.Application(tokens))
                  case result => return result
                }
              // References to operators can be applied
              case Some(_: Token.Operator) | Some(_: Token.Sign) | Some(_: Token.Post) if tokens.length >= 2 && tokens(tokens.length - 2) == Token.Reference =>
                this.tokenizePart(source, Set(CatCode.Close), Set()) match {
                  case Result.Value((tokens: TokenStream, _)) => emit(Token.Application(tokens))
                  case result => return result
                }
              case _ =>
                this.tokenizePart(source, Set(CatCode.Close), Set()) match {
                  case Result.Value((tokens: TokenStream, _)) => emit(Token.Group(tokens))
                  case result => return result
                }
            }
          case CatCode.StartPrimary => this.tokenizePart(source, Set(CatCode.End, CatCode.EndMatch), Set()) match {
            case Result.Value((bracketTokens: TokenStream, _)) if bracketTokens.tokens.nonEmpty && tokens.lastOption.contains(Token.Reference) => return Result.Error("Bracket reference can't contain elements.")
            case Result.Value((bracketTokens: TokenStream, Some(closingToken: String))) => emit(Token.PrimaryBracket(Util.makeString(content), closingToken, bracketTokens))
            case Result.Value(_) => return Result.Error("Could not tokenize input: Closed expression was gobbled without an end token when parsing primary. This should not happen.")
            case result => return result
          }
          case CatCode.StartSecondary => this.tokenizePart(source, Set(CatCode.End, CatCode.EndMatch), Set()) match {
            case Result.Value((bracketTokens: TokenStream, _)) if bracketTokens.tokens.nonEmpty && tokens.lastOption.contains(Token.Reference) => return Result.Error("Bracket reference can't contain elements.")
            case Result.Value((bracketTokens: TokenStream, Some(closingToken: String))) => emit(Token.SecondaryBracket(Util.makeString(content), closingToken, bracketTokens))
            case Result.Value(_) => return Result.Error("Could not tokenize input: Closed expression was gobbled without an end token when parsing secondary. This should not happen.")
            case result => return result
          }
          case CatCode.StartTertiary => this.tokenizePart(source, Set(CatCode.End, CatCode.EndMatch), Set()) match {
            case Result.Value((bracketTokens: TokenStream, _)) if bracketTokens.tokens.nonEmpty && tokens.lastOption.contains(Token.Reference) => return Result.Error("Bracket reference can't contain elements.")
            case Result.Value((bracketTokens: TokenStream, Some(closingToken: String))) => emit(Token.TertiaryBracket(Util.makeString(content), closingToken, bracketTokens))
            case Result.Value(_) => return Result.Error("Could not tokenize input: Closed expression was gobbled without an end token when parsing tertiary. This should not happen.")
            case result => return result
          }
          case CatCode.StartMatch => this.tokenizePart(source, Set(CatCode.EndMatch), Set(), canEmitFollow = true) match {
            case Result.Value((matchTokens: TokenStream, _)) => emit(Token.Match(matchTokens))
            case result => return result
          }
          case CatCode.Close | CatCode.End | CatCode.EndMatch if closingCatCodes.isEmpty => return Result.Error("Dangling token: " + code)
          case CatCode.Close | CatCode.End | CatCode.EndMatch => return Result.Error("Wrong closing token, expected one of " + closingCatCodes.mkString(", ") + ", got " + code)
          case CatCode.Lambda => this.tokenizePart(source, Set(CatCode.Follow), Set()) match {
            case Result.Value((argumentTokens: TokenStream, _)) => this.tokenizePart(source, Set(), Lexer.LambdaTerminators) match {
              case Result.Value((definitionTokens: TokenStream, _)) => emit(Token.Lambda(argumentTokens, definitionTokens))
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
    case Left(string) => Left(UNESCAPE.translate(string))
    case Right(result) => Right(result)
  }
  
  private def tokenizeInterpolatedStringUntil(source: CharacterSource, code: CatCode): Either[Token.Error, Result[Nothing]] = tokenizeStringWithoutEscaping(source, code) match {
    case Left(string) =>
      val source: CharacterSource = new CharacterSource(Util.decomposeString(string))
      val parts: ListBuffer[(String, String)] = ListBuffer()
      var interpolateName: String = ""
      val sb: StringBuilder = new StringBuilder()
      while (true) source.lookupToken(0) match {
        case None =>
          parts.addOne((interpolateName, UNESCAPE.translate(sb.toString())))
          return Left(Token.Error(UNESCAPE.translate(string), parts.head._2, parts.tail.toVector))
        case Some(currentChar) if sb.nonEmpty && sb.last == '\\' && (sb.toString.reverseIterator.takeWhile(_ == '\\').length % 2) != 0 =>
          sb.append(Character.toString(currentChar))
          source.advance(1)
        case Some(currentChar) => this.catCodes.tokCode(source) match {
          case CharacterMapping(CatCode.Interpolate, content) =>
            source.advance(content.length)
            this.catCodes.tokCode(source) match {
              case CharacterMapping(CatCode.Letter | CatCode.Exp, _) =>
                parts.addOne((interpolateName, UNESCAPE.translate(sb.toString())))
                sb.clear()
                interpolateName = consumeIdentifierFromSource(source)
              case _ => sb.append(Util.makeString(content))
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
          try {
            return Left(sb.toString())
          } catch {
            case e: IllegalArgumentException => return Right(Result.Error(e.getMessage))
          }
        case _ =>
          sb.append(Character.toString(currentChar))
          source.advance(1)
      }
    }
    throw new Error()
  }
  
  private def consumeIdentifierFromSource(source: CharacterSource): String = {
    val sb: StringBuilder = new StringBuilder()
    while (true) this.catCodes.tokCode(source) match {
      case CharacterMapping(code, content) if code == CatCode.Letter || code == CatCode.Digit || code == CatCode.Exp =>
        source.advance(content.length)
        sb.append(Util.makeString(content))
      case _ => return sb.toString
    }
    throw new Error()
  }

  private class ImmediateError(val error: Result.Error) extends Exception
}
