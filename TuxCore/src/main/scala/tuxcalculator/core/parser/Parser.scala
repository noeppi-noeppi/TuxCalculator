package tuxcalculator.core.parser

import org.apache.commons.text.StringEscapeUtils
import tuxcalculator.core.expression.Ast
import tuxcalculator.core.lexer.{Token, TokenStream}
import tuxcalculator.core.util.{Result, Util}
import tuxcalculator.core.value.{MathError, MathValue}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

trait ParsingContext {
  def parseNumber(integral: String, fraction: Option[String], exponent: Option[String]): Result[MathValue]
}

class Parser(val ctx: ParsingContext) {
  
  private[this] val parsers: CalculatorParsers = new CalculatorParsers(ctx)
  
  private def wrap[T](res: parsers.ParseResult[T]): Result[T] = res match {
    case parsers.Success(value, _) => Result.Value(value.asInstanceOf[T])
    case parsers.Failure(msg, _) => Result.Error(msg)
    case parsers.Error(msg, _) => Result.Error(msg)
  }
  
  def expression(tokens: TokenStream): Result[Ast.Expression] = wrap(parsers.parseTokens(parsers.expression, tokens))
  
  def letCommand(tokens: TokenStream): Result[Ast.LetCommand] = wrap(parsers.parseTokens(parsers.cmd_let, tokens))
  def defCommand(tokens: TokenStream): Result[Ast.DefCommand] = wrap(parsers.parseTokens(parsers.cmd_def, tokens))
  def remCommand(tokens: TokenStream): Result[Ast.RemCommand] = wrap(parsers.parseTokens(parsers.cmd_rem, tokens))
  def setCommand(tokens: TokenStream): Result[Ast.SetCommand] = wrap(parsers.parseTokens(parsers.cmd_set, tokens))
  def catCommand(tokens: TokenStream): Result[Ast.CatCommand] = wrap(parsers.parseTokens(parsers.cmd_cat, tokens))
  def tokCommand(tokens: TokenStream): Result[Ast.TokCommand] = wrap(parsers.parseTokens(parsers.cmd_tok, tokens))
  def dumpCommand(tokens: TokenStream): Result[Ast.DumpCommand] = wrap(parsers.parseTokens(parsers.cmd_dump, tokens))
}

class CalculatorParsers(val ctx: ParsingContext) extends Parsers  {
  override type Elem = Token
  
  def parseTokens[T](parser: this.Parser[T], tokens: TokenStream): ParseResult[T] = parser.apply(new TokenReader(tokens.tokens.toList)) match {
    case Success(_, remaining) if !remaining.atEnd => Error("Input not fully consumed", remaining)
    case result => result
  }
  
  def flatAcceptMatch[U](expected: String, f: PartialFunction[Elem, ParseResult[U]]): this.Parser[U] = Parser {
    case in if in.atEnd => Failure("end of input", in)
    case in if f.isDefinedAt(in.first) => f.apply(in.first) match {
      case Success(result, _) => Success(result, in.rest)
      case Failure(result, _) => Failure(result, in.rest)
      case result => result
    }
    case in => Failure(expected + " expected", in)
  }
  
  def leftRec[A, B](init: this.Parser[A], follow: this.Parser[B], combine: (A, B) => A): this.Parser[A] = {
    init ~ rep(follow) ^^ {
      case head ~ elems => elems.foldLeft(head)(combine)
    }
  }
  
  def leftRec1N[A, B](init: this.Parser[A], immediateFollow: this.Parser[B], follow: this.Parser[B], combine: (A, B) => A): this.Parser[A] = {
    init ~ opt(immediateFollow ~ rep(follow)) ^^ {
      case head ~ Some(first ~ others) => others.foldLeft(combine(head, first))(combine)
      case head ~ None => head
    }
  }
  
  // Utilities
  def identifier: this.Parser[String] = acceptMatch("identifier", {
    case Token.Identifier(name) => name
  })
  
  private def parameter: this.Parser[(String, Boolean)] = identifier ~ opt(accept(Token.Vararg)) ^^ {
    case name ~ Some(_) => (name, true)
    case name ~ None => (name, false)
  }
  def signature: this.Parser[Ast.Signature] = { repsep(parameter, accept(Token.ElementSep)) ^^ (x => x.toVector) } ^? ({
    case Vector() => Ast.Signature(Vector(), vararg = false)
    case args if args.forall(!_._2) => Ast.Signature(args.map(_._1), vararg = false)
    case args if args.init.forall(!_._2) => Ast.Signature(args.map(_._1), vararg = true)
  }, _ => "Only the last parameter can be vararg")

  def guard: this.Parser[Option[Ast.Expression]] = opt(accept(Token.Guard) ~> expression)
  private def guarded_parameter: this.Parser[(String, Boolean, Option[Ast.Expression])] = parameter ~ guard ^^ {
    case (name, vararg) ~ guard => (name, vararg, guard)
  }
  def guarded_signature: this.Parser[(Ast.Signature, Vector[Option[Ast.Expression]])] = { repsep(guarded_parameter, accept(Token.ElementSep)) ^^ (x => x.toVector) } ^? ({
    case Vector() => (Ast.Signature(Vector(), vararg = false), Vector())
    case args if args.forall(!_._2) => (Ast.Signature(args.map(_._1), vararg = false), args.map(_._3))
    case args if args.init.forall(!_._2) => (Ast.Signature(args.map(_._1), vararg = true), args.map(_._3))
  }, _ => "Only the last parameter can be vararg")
  
  private def target_func: this.Parser[Ast.DefTarget] = identifier ^^ (name => Ast.DefTarget.Function(name))
  private def target_other: this.Parser[Ast.DefTarget] = flatAcceptMatch("def operator", {
    case Token.Operator(name) => Success(Ast.DefTarget.Operator(name), TokenReader.Empty)
    case Token.Sign(name) => Success(Ast.DefTarget.SignOrOperator(name), TokenReader.Empty)
    case Token.Post(name) => Success(Ast.DefTarget.Post(name), TokenReader.Empty)
    case Token.PrimaryBracket(_, _, tokens) if tokens.tokens.nonEmpty => Failure("No tokens are allowed in bracket reference.", TokenReader.Empty)
    case Token.PrimaryBracket(open, close, _) => Success(Ast.DefTarget.PrimaryBracket(open, close), TokenReader.Empty)
    case Token.SecondaryBracket(_, _, tokens) if tokens.tokens.nonEmpty => Failure("No tokens are allowed in bracket reference.", TokenReader.Empty)
    case Token.SecondaryBracket(open, close, _) => Success(Ast.DefTarget.SecondaryBracket(open, close), TokenReader.Empty)
    case Token.TertiaryBracket(_, _, tokens) if tokens.tokens.nonEmpty => Failure("No tokens are allowed in bracket reference.", TokenReader.Empty)
    case Token.TertiaryBracket(open, close, _) => Success(Ast.DefTarget.TertiaryBracket(open, close), TokenReader.Empty)
    case _ => Failure("identifier or operator expected", TokenReader.Empty)
  })

  private def target: this.Parser[Ast.DefTarget] = target_func | target_other
  
  // Basic values
  def value: this.Parser[Ast.Expression] = flatAcceptMatch("struct", {
    case Token.Number(integral, fraction, exponent) => ctx.parseNumber(integral, fraction, exponent) match {
      case Result.Value(num) => Success(Ast.Value(num), TokenReader.Empty)
      case Result.Error(msg) => Error(msg, TokenReader.Empty)
    }
    case Token.Answer => Success(Ast.Answer, TokenReader.Empty)
    case Token.Error(msg) => Success(Ast.Value(MathError(msg)), TokenReader.Empty)
    case Token.Group(tokens) => parseTokens(expression, tokens).map(expr => Ast.Group(expr))
    case Token.PrimaryBracket(open, close, tokens) => parseTokens(expression, tokens).map(expr => Ast.PrimaryBracket(open, close, expr))
    case Token.SecondaryBracket(open, close, tokens) => parseTokens(argument_list, tokens).map(expr => Ast.SecondaryBracket(open, close, expr.toVector))
    case Token.TertiaryBracket(open, close, tokens) => parseTokens(expression_grouped_list, tokens).map(expr => Ast.TertiaryBracket(open, close, expr.map(_.toVector).toVector))
    case Token.Match(tokens) => parseTokens(repsep(match_entry, accept(Token.GroupSep)), tokens).map(args => Ast.Match(args.toVector))
    case Token.Lambda(args, tokens) => parseTokens(signature, args) match {
      case Success(sig, _) => parseTokens(expression, tokens).map(expr => Ast.Lambda(sig, expr))
      case result @ Failure(_, _) => result
      case result @ Error(_, _) => result
    }
  })
  
  private def match_entry: this.Parser[Ast.MatchEntry] = flatAcceptMatch("guarded argument list", {
    case Token.Group(tokens) => parseTokens(guarded_signature, tokens)
  }) ~ guard ~ accept(Token.Follow) ~ expression ^^ {
    case (sig, elementGuards) ~ mainGuard ~ _ ~ expr => Ast.MatchEntry(sig, elementGuards.map(_.map(ex => Ast.DefExpression(ex, ex))), mainGuard.map(ex => Ast.DefExpression(ex, ex)), expr, expr)
  }
  
  def variable: this.Parser[Ast.Variable] = identifier ^^ (name => Ast.Variable(name))
  def reference: this.Parser[Ast.Reference] = accept(Token.Reference) ~> target ^^ (name => Ast.Reference(name))
  def special: this.Parser[Ast.Special] = accept(Token.Special) ~> identifier ^^ (name => Ast.Special(name))
  
  def basic_expression: this.Parser[Ast.Expression] = value | variable | reference | special | failure("expression expected")
  
  def post_action_apply: this.Parser[PostOperation.Application] = flatAcceptMatch("application", {
    case Token.Application(tokens) => parseTokens(partial_argument_list, tokens).map(args => PostOperation.Application(args.toVector))
  })
  def post_action_partial_apply_direct: this.Parser[PostOperation.PartialApplication] = accept(Token.PartialApplication) ~> flatAcceptMatch("partial application", {
    case Token.Application(tokens) => parseTokens(partial_argument_list, tokens).map(args => PostOperation.PartialApplication(args.toVector))
  })
  def post_action_partial_apply_simple: this.Parser[PostOperation.PartialApplication] = accept(Token.PartialApplication) ~> opt(acceptMatch("sign", { case Token.Sign(name) => name })) ~ (value | variable) ^^ {
    case Some(sgn) ~ value => PostOperation.PartialApplication(Vector(Ast.SignApplication(sgn, value)))
    case None ~ value => PostOperation.PartialApplication(Vector(value))
  }
  def post_action_any_apply: this.Parser[PostOperation] = post_action_partial_apply_direct | post_action_partial_apply_simple | post_action_apply
  def applied_expression: this.Parser[Ast.Expression] = leftRec[Ast.Expression, PostOperation](basic_expression, post_action_any_apply, (base, apply) => (base, apply) match {
    case (Ast.Variable(name), PostOperation.Application(app)) => Ast.Invocation(name, app)
    case (Ast.Variable(name), PostOperation.PartialApplication(app)) => Ast.PartialInvocation(name, app)
    case (expr, PostOperation.Application(app)) => Ast.Application(expr, app)
    case (expr, PostOperation.PartialApplication(app)) => Ast.PartialApplication(expr, app)
    case _ => throw new IllegalStateException("Parser error: Encountered invalid post operation type while parsing applications. This is a bug.")
  })
  
  def signed_expression: this.Parser[Ast.Expression] = explicitly_signed_expression | applied_expression
  def explicitly_signed_expression: this.Parser[Ast.Expression] = acceptMatch("sign", { case Token.Sign(name) => name }) ~ signed_expression ^^ {
    case sgn ~ expr => Ast.SignApplication(sgn, expr)
  }
  
  def post_action_op: this.Parser[PostOperation] = acceptMatch("postfix", { case Token.Post(name) => PostOperation.Operator(name) })
  def post_action: this.Parser[PostOperation] = post_action_any_apply | post_action_op
  def post_expression: this.Parser[Ast.Expression] = leftRec1N[Ast.Expression, PostOperation](signed_expression, post_action_op, post_action, (base, apply) => (base, apply) match {
    case (expr, PostOperation.Application(app)) => Ast.Application(expr, app)
    case (expr, PostOperation.PartialApplication(app)) => Ast.PartialApplication(expr, app)
    case (expr, PostOperation.Operator(name)) => Ast.PostApplication(name, expr)
  })
  
  // Somewhat hacky parser to try out the longest possible prefix followed by every shorter one
  // down to the no-token prefix.
  def shorthand_expression: this.Parser[Ast.Expression] = (in: Input) => {
    // calls list is in reverse
    case class Possibility(calls: List[String], rest: Input)
    def findPossibilities(calls: List[String], inp: Input): List[Possibility] = inp.first match {
      case Token.Identifier(name) => Possibility(calls, inp) :: findPossibilities(name :: calls, inp.rest)
      case _ => Possibility(calls, inp) :: Nil
    }
    val possibilities: List[Possibility] = findPossibilities(Nil, in)
    def solution(possibilities: List[Possibility]): ParseResult[Ast.Expression] = possibilities match {
      case h :: t => solution(t) match {
        case r @ Success(_, _) => r
        case _ => post_expression(h.rest) match {
          case Success(expr, nextInp) => Success(h.calls.foldLeft(expr)((expr, name) => Ast.ShorthandInvocation(name, expr)), nextInp)
          case fail => fail
        }
      }
      case Nil => Failure("No shorthand", in)
    }
    solution(possibilities)
  }
  
  def operator_application: this.Parser[(String, Ast.Expression)] = acceptMatch("operator", {
    case Token.Operator(op) => op
  }) ~ shorthand_expression ^^ { case op ~ expr => (op, expr) }
  
  def operator_expression: this.Parser[Ast.Expression] = shorthand_expression ~ rep(operator_application) ^^ {
    case expr ~ Nil => expr
    case expr ~ ops => Ast.OperatorApplication(expr, ops.toVector)
  }
  
  def expression: this.Parser[Ast.Expression] = operator_expression
  def expression_list: this.Parser[List[Ast.Expression]] = repsep(expression, accept(Token.ElementSep))
  def expression_grouped_list: this.Parser[List[List[Ast.Expression]]] = repsep(expression_list, accept(Token.GroupSep))
  
  def argument: this.Parser[Ast.Argument] = expression ~ opt(accept(Token.Vararg)) ^^ {
    case expr ~ Some(_) => Ast.SplattedArgument(expr)
    case expr ~ None => expr
  }
  def argument_list: this.Parser[List[Ast.Argument]] = repsep(argument, accept(Token.ElementSep))

  def partial_argument: this.Parser[Ast.PartialArgument] = argument | (accept(Token.PartialApplication) ^^ (_ => Ast.Placeholder))
  def partial_argument_list: this.Parser[List[Ast.PartialArgument]] = repsep(partial_argument, accept(Token.ElementSep))
  
  def cmd_let: this.Parser[Ast.LetCommand] = identifier ^^ (name => Ast.LetCommand(name))
  def cmd_rem: this.Parser[Ast.RemCommand] = target ^^ (name => Ast.RemCommand(name))
  def cmd_set: this.Parser[Ast.SetCommand] = identifier ^^ (name => Ast.SetCommand(name))

  private def cmd_def_priority: this.Parser[Ast.Expression] = flatAcceptMatch("priority", {
    case Token.Group(tokens) => parseTokens(expression, tokens)
    case Token.Application(tokens) => parseTokens(expression, tokens)
  })
  def cmd_def: this.Parser[Ast.DefCommand] = opt(cmd_def_priority) ~ target ~ flatAcceptMatch("argument list", {
    case Token.Group(tokens) => parseTokens(signature, tokens)
    case Token.Application(tokens) => parseTokens(signature, tokens)
  }) ^^ { case priority ~ name ~ sig => Ast.DefCommand(name, priority, sig) }
  
  def cmd_cat: this.Parser[Ast.CatCommand] = flatAcceptMatch("character", {
    case Token.Error(msg) => Util.decomposeString(msg) match {
      case Vector(codePoint) => Success(codePoint, TokenReader.Empty)
      case _ => Failure("expected a single character, got '" + StringEscapeUtils.escapeJava(msg) + "'", TokenReader.Empty)
    }
  }) ^^ (codePoint => Ast.CatCommand(codePoint))
  def cmd_tok: this.Parser[Ast.TokCommand] = flatAcceptMatch("token", {
    case Token.Error(msg) => msg match {
      case "" => Failure("expected a token, got empty string", TokenReader.Empty)
      case _ => Success(msg, TokenReader.Empty)
    }
  }) ^^ (token => Ast.TokCommand(token))
  def cmd_dump: this.Parser[Ast.DumpCommand] = flatAcceptMatch("file name", {
    case Token.Error(msg) => msg match {
      case "" => Failure("expected a file name, got empty string", TokenReader.Empty)
      case _ => Success(msg, TokenReader.Empty)
    }
  }) ^^ (token => Ast.DumpCommand(token))
}

sealed trait PostOperation
object PostOperation {
  case class Operator(name: String) extends PostOperation
  case class Application(args: Vector[Ast.PartialArgument]) extends PostOperation
  case class PartialApplication(args: Vector[Ast.PartialArgument]) extends PostOperation
}

class TokenReader(private val tokens: List[Token]) extends Reader[Token] {
  override lazy val first: Token = tokens match {
    case head :: _ => head
    case Nil => Token.Eof
  }
  
  override lazy val rest: Reader[Token] = tokens match {
    case _ :: tail => new TokenReader(tail)
    case Nil => this
  }

  override def pos: Position = NoPosition
  override def atEnd: Boolean = tokens.isEmpty
}

object TokenReader {
  val Empty = new TokenReader(Nil)
}
