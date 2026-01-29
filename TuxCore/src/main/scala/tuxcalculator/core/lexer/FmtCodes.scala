package tuxcalculator.core.lexer

import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.translate.{AggregateTranslator, CharSequenceTranslator, LookupTranslator}

import java.util.Locale
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object FmtCode extends Enumeration {
  val DecimalSep: FmtCode = Value(CatCode.DecimalSep)
  val ElementSep: FmtCode = Value(CatCode.ElementSep)
  val GroupSep: FmtCode = Value(CatCode.GroupSep)
  val True: FmtCode = Value("true")
  val False: FmtCode = Value("false")
  val Exp: FmtCode = Value(CatCode.Exp)
  val Assign: FmtCode = Value(CatCode.Assign)
  val Reference: FmtCode = Value(CatCode.Reference)
  val Error: FmtCode = Value(CatCode.Error)
  val Open: FmtCode = Value(CatCode.Open)
  val Close: FmtCode = Value(CatCode.Close)
  val StartList: FmtCode = Value("start list")
  val EndList: FmtCode = Value("end list")
  val StartMatrix: FmtCode = Value("start matrix")
  val EndMatrix: FmtCode = Value("end matrix")
  val StartMatch: FmtCode = Value(CatCode.StartMatch)
  val EndMatch: FmtCode = Value(CatCode.EndMatch)
  val Lambda: FmtCode = Value(CatCode.Lambda)
  val Follow: FmtCode = Value(CatCode.Follow)
  val Guard: FmtCode = Value(CatCode.Guard)
  val Special: FmtCode = Value(CatCode.Special)
  val VarArg: FmtCode = Value(CatCode.VarArg)
  val Partial: FmtCode = Value(CatCode.Partial)
  val Answer: FmtCode = Value(CatCode.Answer)
  val Variable: FmtCode = Value("variable")
  val Imaginary: FmtCode = Value("imaginary")
  val SelfReference: FmtCode = Value("self reference")
  val Angle: FmtCode = Value("angle")
  val Degree: FmtCode = Value("degree")
  val Truncate: FmtCode = Value("truncate")

  private[this] def Value(catCode: CatCode): Value = Value(catCode.toString)

  def byName(name: String): Option[FmtCode] = try {
    Some(withName(name.strip().toLowerCase(Locale.ROOT)))
  } catch {
    case _: NoSuchElementException => None
  }
}

class FmtCodes {
  private[this] val fmtCodes: mutable.Map[FmtCode, String] = mutable.Map()
  private[this] var escape: CharSequenceTranslator = this.escapeTranslator

  this.fmtCodes(FmtCode.Error) = "\""
  this.fmtCodes(FmtCode.Special) = "#"
  this.fmtCodes(FmtCode.Assign) = "="

  private def defaultFmtCode(code: FmtCode): String = code match {
    case FmtCode.Error => "\""
    case FmtCode.Special => "#"
    case FmtCode.Assign => "="
    case _ => ""
  }

  def fmtCode(code: FmtCode): String = this.fmtCodes.getOrElse(code, "")
  def fmtCode(code: FmtCode, format: String): Unit = {
    this.fmtCodes(code) = format
    this.escape = this.escapeTranslator
  }

  def escError(msg: String): String = this.escape.translate(msg)
  
  def allChangedFmtCodes: Map[FmtCode, String] = this.fmtCodes.toMap.filter(entry => {
    val (fmtCode, format) = entry
    format != defaultFmtCode(fmtCode)
  })
  
  private def escapeTranslator: CharSequenceTranslator = {
    val translateMap: Map[CharSequence, CharSequence] = fmtCode(FmtCode.Error) match {
      case "" => Map("'" -> "\\'")
      case error => Map("'" -> "\\'", error -> ("\\" + error))
    }
    new AggregateTranslator(
      new LookupTranslator(translateMap.asJava),
      StringEscapeUtils.ESCAPE_JAVA
    )
  }
}
