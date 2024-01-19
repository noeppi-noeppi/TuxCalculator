package tuxcalculator.core.lexer

sealed trait Token
case object Token {
  case class Identifier(name: String) extends Token
  case class Number(integral: String, fraction: Option[String], exponent: Option[String]) extends Token
  case object ElementSep extends Token
  case object GroupSep extends Token
  case class Operator(name: String) extends Token
  case class Sign(name: String) extends Token
  case class Post(name: String) extends Token
  case object Reference extends Token
  case class Error(nonInterpolatedString: String, interpolatedHead: String, interpolatedTail: Vector[(String, String)]) extends Token
  case class Application(tokens: TokenStream) extends Token
  case class Group(tokens: TokenStream) extends Token
  case class PrimaryBracket(open: String, close: String, tokens: TokenStream) extends Token
  case class SecondaryBracket(open: String, close: String, tokens: TokenStream) extends Token
  case class TertiaryBracket(open: String, close: String, tokens: TokenStream) extends Token
  case class Match(tokens: TokenStream) extends Token
  case class Lambda(declaration: TokenStream, definition: TokenStream) extends Token
  case object Follow extends Token
  case object Guard extends Token
  case object Special extends Token
  case object Vararg extends Token
  case object PartialApplication extends Token
  case object Answer extends Token
  case object Eof extends Token
}
