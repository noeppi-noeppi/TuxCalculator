package tuxcalculator.core.lexer

import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.translate.{AggregateTranslator, LookupTranslator}
import tuxcalculator.core.lexer.CatCode.CatCode
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
    CatCode.Close, CatCode.End, CatCode.ElementSep, CatCode.GroupSep, CatCode.VarArg, CatCode.Follow
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
    tokenizePart(source, None, Set())
  } catch {
    case e: ImmediateError => e.error
  }
  
  def tokenizeAssignment(line: String): Result[PartialTokenStream] = try {
    val source: CharacterSource = new CharacterSource(Util.decomposeString(line))
    val result = tokenizePart(source, None, Set(), tokenizeAssignment = true)
    result ~ (tokens => PartialTokenStream(tokens, source.remaining))
  } catch {
    case e: ImmediateError => e.error
  }
  
  @throws[ImmediateError]
  private def tokenizePart(source: CharacterSource, closingCatCode: Option[CatCode], breakAt: Set[CatCode], tokenizeAssignment: Boolean = false, canEmitFollow: Boolean = false): Result[TokenStream] = {
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
                   | Some(_: Token.Group) | Some(_: Token.Application) | Some(_: Token.List) | Some(_: Token.Vector)
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
          // We got a decimal point but no fractional part. This is an error
          throw new ImmediateError(Result.Error("Dangling decimal separator"))
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
    def finish(isAssignToken: Boolean = false): Result[TokenStream] = {
      CatCodeGrouper.finish()
      if (tokenizeAssignment && !isAssignToken) {
        Result.Error("Input ended prematurely, expected an assignment token.")
      } else if (!tokenizeAssignment && isAssignToken) {
        Result.Error("Invalid catcode configuration.")
      } else {
        Result.Value(TokenStream(tokens.toVector))
      }
    }

    while (true) this.catCodes.tokCode(source) match {
      case TokResult.Eof if closingCatCode.isEmpty => return finish()
      case TokResult.Eof => return Result.Error("Closed expression is not terminated. Expected " + closingCatCode.get)
      case TokResult.Ambiguity(matches) => return Result.Error("Ambiguous tok-codes: Multiple matches: " + matches.mkString(", "))
      case CharacterMapping(code, content) => 
        // If there is the possibility to break before a lookahead catcode, we must always break when a comment starts
        // or we might actually read after the comment.
        if (breakAt.contains(code) || (breakAt.nonEmpty && code == CatCode.Comment)) return finish()
        source.advance(content.length)
        code match {
          case _ if closingCatCode.contains(code) => return finish()
          case CatCode.Assign if tokenizeAssignment && !CatCodeGrouper.isGroupingOperator => return finish(isAssignToken = true)
          case CatCode.Letter | CatCode.Digit | CatCode.DecimalSep | CatCode.Exp | CatCode.Operator | CatCode.Assign | CatCode.Sign | CatCode.Post => CatCodeGrouper.consume(code, content) match {
            case Some(result) => return result
            case None =>
          }
          case CatCode.Space => CatCodeGrouper.finish()
          case CatCode.ElementSep => emit(Token.ElementSep)
          case CatCode.GroupSep => emit(Token.GroupSep)
          case CatCode.Reference => emit(Token.Reference)
          case CatCode.Error => this.tokenizeStringUntil(source, CatCode.Error) match {
            case Left(string) => emit(Token.Error(string))
            case Right(result) => return result;
          }
          case CatCode.Open =>
            CatCodeGrouper.finish()
            tokens.lastOption match {
              case Some(_: Token.Identifier) | Some(_: Token.Number) | Some(_: Token.Post) | Some(_: Token.Application)
                   | Some(_: Token.Group) | Some(_: Token.Error) | Some(_: Token.List) | Some(_: Token.Match)
                   | Some(_: Token.Vector) | Some(_: Token.Lambda) | Some(Token.PartialApplication) | Some(Token.Answer) =>
                this.tokenizePart(source, Some(CatCode.Close), Set()) match {
                  case Result.Value(tokens: TokenStream) => emit(Token.Application(tokens))
                  case result => return result
                }
              // References to operators can be applied
              case Some(_: Token.Operator) | Some(_: Token.Sign) | Some(_: Token.Post) if tokens.length >= 2 && tokens(tokens.length - 2) == Token.Reference =>
                this.tokenizePart(source, Some(CatCode.Close), Set()) match {
                  case Result.Value(tokens: TokenStream) => emit(Token.Application(tokens))
                  case result => return result
                }
              case _ =>
                this.tokenizePart(source, Some(CatCode.Close), Set()) match {
                  case Result.Value(tokens: TokenStream) => emit(Token.Group(tokens))
                  case result => return result
                }
            }
          case CatCode.StartList => this.tokenizePart(source, Some(CatCode.End), Set()) match {
            case Result.Value(tokens: TokenStream) => emit(Token.List(tokens))
            case result => return result
          }
          case CatCode.StartVector => this.tokenizePart(source, Some(CatCode.End), Set()) match {
            case Result.Value(tokens: TokenStream) => emit(Token.Vector(tokens))
            case result => return result
          }
          case CatCode.StartMatch => this.tokenizePart(source, Some(CatCode.End), Set(), canEmitFollow = true) match {
            case Result.Value(tokens: TokenStream) => emit(Token.Match(tokens))
            case result => return result
          }
          case CatCode.Close | CatCode.End if closingCatCode.isEmpty => return Result.Error("Dangling token: " + code)
          case CatCode.Close | CatCode.End => return Result.Error("Wrong closing token, expected " + closingCatCode.get + ", got " + code)
          case CatCode.Lambda => this.tokenizePart(source, Some(CatCode.Follow), Set()) match {
            case Result.Value(argumentTokens: TokenStream) => this.tokenizePart(source, None, Lexer.LambdaTerminators) match {
              case Result.Value(definitionTokens: TokenStream) => emit(Token.Lambda(argumentTokens, definitionTokens))
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
          case CatCode.Comment if closingCatCode.isEmpty => return finish()
          case CatCode.Comment => return Result.Error("Comment inside closed expression")
          case CatCode.VarArg => emit(Token.Vararg)
          case CatCode.Partial => emit(Token.PartialApplication)
          case CatCode.Answer => emit(Token.Answer)
          case CatCode.Invalid => content.headOption match {
            case Some(char) => return Result.Error("Invalid character: " + Character.getName(char))
            case None => return Result.Error("Invalid tok-code")
          }
        }
    }
    throw new Error()
  }
  
  private def tokenizeStringUntil(source: CharacterSource, code: CatCode): Either[String, Result[Nothing]] = {
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
            return Left(UNESCAPE.translate(sb.toString()))
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

  private class ImmediateError(val error: Result.Error) extends Exception
}
