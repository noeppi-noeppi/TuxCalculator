package tuxcalculator.core.lexer

import java.util.Locale
import scala.collection.mutable

object CatCode extends Enumeration {
  val Letter: CatCode = Value("letter")
  val Space: CatCode = Value("space")
  val Digit: CatCode = Value("digit")
  val DecimalSep: CatCode = Value("decimal sep")
  val ElementSep: CatCode = Value("element sep")
  val GroupSep: CatCode = Value("group sep")
  val Exp: CatCode = Value("exp")
  val Operator: CatCode = Value("operator")
  val Assign: CatCode = Value("assign")
  val Sign: CatCode = Value("sign")
  val Post: CatCode = Value("post")
  val Reference: CatCode = Value("reference")
  val Error: CatCode = Value("error")
  val Open: CatCode = Value("open")
  val Close: CatCode = Value("close")
  val StartPrimary: CatCode = Value("start primary")
  val StartSecondary: CatCode = Value("start secondary")
  val StartTertiary: CatCode = Value("start tertiary")
  val End: CatCode = Value("end")
  val StartMatch: CatCode = Value("start match")
  val EndMatch: CatCode = Value("end match")
  val Lambda: CatCode = Value("lambda")
  val Follow: CatCode = Value("follow")
  val Guard: CatCode = Value("guard")
  val Escape: CatCode = Value("escape")
  val Special: CatCode = Value("special")
  val Comment: CatCode = Value("comment")
  val VarArg: CatCode = Value("vararg")
  val Partial: CatCode = Value("partial")
  val Answer: CatCode = Value("answer")
  val Interpolate: CatCode = Value("interpolate")
  val Invalid: CatCode = Value("invalid")
  
  def byName(name: String): Option[CatCode] = try {
    Some(withName(name.strip().toLowerCase(Locale.ROOT)))
  } catch {
    case _: NoSuchElementException => None
  }
}

trait Lookahead[T] {
  def lookupToken(ahead: Int): Option[T]
  def offset(offset: Int): Lookahead[T] = (ahead: Int) => Lookahead.this.lookupToken(offset + ahead)
}

sealed trait TokResult
case class CharacterMapping(code: CatCode, content: Vector[Int]) extends TokResult
object TokResult {
  case object Eof extends TokResult
}

class CatCodes {
  private[this] val catCodes: mutable.Map[Int, CatCode] = mutable.Map()
  private[this] val tokCodes: mutable.Map[Vector[Int], CatCode] = mutable.Map()
  private[this] val escapeCharacters: mutable.Set[Int] = mutable.Set()
  
  for (i <- '0' to '9') this.catCodes(i) = CatCode.Digit
  this.catCodes('\'') = CatCode.Error
  this.catCodes('#') = CatCode.Special
  this.catCodes('=') = CatCode.Assign
  
  private def defaultCatCode(codePoint: Int): CatCode = codePoint match {
    case _ if codePoint >= '0' && codePoint <= '9' => CatCode.Digit
    case '\'' => CatCode.Error
    case '#' => CatCode.Special
    case '=' => CatCode.Assign
    case _ if Character.isLetter(codePoint) => CatCode.Letter
    case _ if Character.isSpaceChar(codePoint) || Character.isWhitespace(codePoint) => CatCode.Space
    case _ => CatCode.Invalid
  }
  
  def catCode(codePoint: Int): CatCode = codePoint match {
    case this.catCodes(code) => code
    case _ => this.defaultCatCode(codePoint)
  }

  def tokCode(source: Lookahead[Int]): TokResult = {
    def tokMatches(tok: Vector[Int]): Boolean = tok.zipWithIndex.forall {
      case (codePoint, idx) => source.lookupToken(idx).contains(codePoint)
    }
    val matchingTokens: Set[Vector[Int]] = tokCodes.keySet.filter(tokMatches).toSet
    matchingTokens.size match {
      case 0 => source.lookupToken(0) match {
        case Some(codePoint) => CharacterMapping(this.catCode(codePoint), Vector(codePoint))
        case None => TokResult.Eof
      }
      case 1 => CharacterMapping(this.tokCodes(matchingTokens.head), matchingTokens.head)
      case _ =>
        val longestMatchingToken = matchingTokens.maxBy(_.length)
        CharacterMapping(this.tokCodes(longestMatchingToken), longestMatchingToken)
    }
  }

  def catCode(codePoint: Int, code: CatCode): Unit = {
    if (this.catCodes.get(codePoint).contains(CatCode.Escape)) this.escapeCharacters.remove(codePoint)
    this.catCodes(codePoint) = code
    if (code == CatCode.Escape) this.escapeCharacters.add(codePoint)
  }
  def tokCode(token: Vector[Int], code: CatCode): Unit = code match {
    case CatCode.Invalid => this.tokCodes.remove(token)
    case _ => this.tokCodes(token) = code
  }
  
  def escapeCodePoints: Set[Int] = escapeCharacters.toSet

  def allChangedCatCodes: Map[Int, CatCode] = this.catCodes.toMap.filter(entry => {
    val (codePoint, catCode) = entry
    catCode != defaultCatCode(codePoint)
  })
  
  def allChangedTokCodes: Map[Vector[Int], CatCode] = this.tokCodes.toMap
}
