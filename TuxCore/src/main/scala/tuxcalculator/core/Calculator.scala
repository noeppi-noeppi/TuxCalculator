package tuxcalculator.core

import ch.obermuhlner.math.big.{BigComplex, BigDecimalMath}
import tuxcalculator.api.TuxFrontend
import tuxcalculator.core.data.{CalculatorCommands, CalculatorProperties, CalculatorSpecials, PropertyAccess}
import tuxcalculator.core.expression.Ast
import tuxcalculator.core.format.FormatIO
import tuxcalculator.core.lexer.{CatCode, FmtCode, Lexer, PartialTokenStream, SplitText, TokenStream}
import tuxcalculator.core.parser.{Parser, ParsingContext}
import tuxcalculator.core.resolution.{BindLogic, ComputationLogic, ResolutionTable}
import tuxcalculator.core.util.{Result, Util}
import tuxcalculator.core.value._

import java.io.DataOutputStream
import java.math.{MathContext, RoundingMode, BigDecimal => BigDec}
import java.text.Normalizer

class Calculator(val frontend: TuxFrontend, val ini: Boolean) extends ParsingContext with PropertyAccess {

  private[this] var ready: Boolean = ini
  private[this] var dumpedUnusable: Boolean = false
  
  val lexer = new Lexer
  val parser = new Parser(this)
  
  val properties: CalculatorProperties = new CalculatorProperties(this, () => {
    specials.propertyChange()
    _mathContext = null
    _outputMathContext = null
    _constantPi = null
  })
  val specials: CalculatorSpecials = new CalculatorSpecials(this)
  val resolution: ResolutionTable = new ResolutionTable(this)
  val commands: CalculatorCommands = new CalculatorCommands(lexer)

  private[this] var _mathContext: MathContext = _
  private[this] var _outputMathContext: MathContext = _
  private[this] var _constantPi: BigDec = _
  private[this] var _answer: MathValue = MathVoid

  def precision: Int = properties(CalculatorProperties.Precision)
  def mathContext: MathContext = {
    if (_mathContext == null) {
      _mathContext = new MathContext(precision, RoundingMode.HALF_UP)
    }
    _mathContext
  }
  
  private def outputMathContext: MathContext = {
    if (_outputMathContext == null) {
      _outputMathContext = properties(CalculatorProperties.Output) match {
        case 0 => new MathContext(precision, RoundingMode.HALF_UP)
        case o => new MathContext(precision min o, RoundingMode.HALF_UP)
      }
    }
    _outputMathContext
  }
  
  private def constantPi: BigDec = {
    if (_constantPi == null) {
      _constantPi = BigDecimalMath.pi(mathContext)
    }
    _constantPi
  }

  def answer: MathValue = _answer
  def finish(answer: MathValue): Unit = if (ready) throw new IllegalStateException("Calculator already properly loaded.") else {
    _answer = answer
    ready = true
  }
  
  private def formatReal(value: BigDecimal, suffix: String = "", allowScientific: Boolean = true, forceSpacedOutSign: Boolean = false): String = {
    if (forceSpacedOutSign) {
      val sign = if (value.signum == -1) " - " else " + "
      sign + formatReal(value.abs, suffix = suffix, allowScientific = allowScientific, forceSpacedOutSign = false)
    } else new BigDecimal(Util.safeRound(value, outputMathContext).bigDecimal, mathContext) match {
      case v if v.abs < 1000000 && v.abs >= 0.0001 => Util.formatPlain(this, Util.safeStripTrailingZeros(v)) + suffix
      case v if v.isWhole && v.abs < 100000000 => Util.formatPlain(this, Util.safeStripTrailingZeros(v)) + suffix
      case v if allowScientific => Util.formatScientific(this, Util.safeStripTrailingZeros(v)) + (if (suffix.nonEmpty) " " + suffix else "")
      case v => Util.formatPlain(this, Util.safeStripTrailingZeros(v)) + suffix
    }
  }
  
  private def formatComplex(value: BigComplex, forceSpacedOutSign: Boolean = false, inMultiplicativeContext: Boolean = false): String = {
    value match {
      case _ if value.isReal => formatReal(value.re, forceSpacedOutSign = forceSpacedOutSign)
      case _ if properties(CalculatorProperties.Polar) == CalculatorProperties.PolarType.Radians =>
        formatReal(value.abs(mathContext), forceSpacedOutSign = forceSpacedOutSign) + format(FmtCode.Angle) + formatReal(value.angle(mathContext), allowScientific = false)
      case _ if properties(CalculatorProperties.Polar) == CalculatorProperties.PolarType.Degrees =>
        formatReal(value.abs(mathContext), forceSpacedOutSign = forceSpacedOutSign) + format(FmtCode.Angle) + formatReal(value.angle(mathContext).multiply(new BigDec("180"), mathContext).divide(constantPi, mathContext), allowScientific = false, suffix = format(FmtCode.Degree))
      case _ if inMultiplicativeContext =>
        val sign = if (value.re.signum == -1) { if (forceSpacedOutSign) " - "  else "-" } else { if (forceSpacedOutSign) " + " else "" }
        val term = format(FmtCode.Open) + formatReal(value.re.abs) + formatReal(value.im.multiply(BigDec.valueOf(value.re.signum)), suffix = format(FmtCode.Imaginary), forceSpacedOutSign = true) + format(FmtCode.Close)
        sign + term
      case _ if inMultiplicativeContext => format(FmtCode.Open) + formatReal(value.re, forceSpacedOutSign = forceSpacedOutSign) + formatReal(value.im, suffix = format(FmtCode.Imaginary), forceSpacedOutSign = true) + format(FmtCode.Close)
      case _ => formatReal(value.re, forceSpacedOutSign = forceSpacedOutSign) + formatReal(value.im, suffix = format(FmtCode.Imaginary), forceSpacedOutSign = true)
    }
  }
  
  private def formatPol(coefficients: Vector[MathNumber]): String = {
    def partString(coefficient: MathNumber, power: Int): Option[String] = {
      val variable: String = if (coefficient.num.isReal) format(FmtCode.Variable) else " " + format(FmtCode.Variable)
      val first = coefficients.size == power + 1
      val plus: String = if (first) "" else " + "
      val minus: String = if (first) "-" else " - "
      power match {
        case _ if Util.safeRound(coefficient.num, outputMathContext) == BigComplex.ZERO => None
        case 0 => Some(formatComplex(coefficient.num, forceSpacedOutSign = !first, inMultiplicativeContext = true))
        case 1 if Util.safeRound(coefficient.num, outputMathContext) == BigComplex.ONE => Some(plus + variable)
        case 1 if Util.safeRound(coefficient.num, outputMathContext) == BigComplex.ONE.negate() => Some(minus + variable)
        case 1 => Some(formatComplex(coefficient.num, forceSpacedOutSign = !first, inMultiplicativeContext = true) + variable)
        case n if Util.safeRound(coefficient.num, outputMathContext) == BigComplex.ONE => Some(plus + variable + Util.toSuperscript(n))
        case n if Util.safeRound(coefficient.num, outputMathContext) == BigComplex.ONE.negate() => Some(minus + variable + Util.toSuperscript(n))
        case n => Some(formatComplex(coefficient.num, forceSpacedOutSign = !first, inMultiplicativeContext = true) + variable + Util.toSuperscript(n))
      }
    }
    coefficients.zipWithIndex.reverse.flatMap(entry => partString(entry._1, entry._2)).mkString
  }

  private def formatNoTrunc(value: MathValue): String = value match {
    case MathVoid => format(FmtCode.Open) + format(FmtCode.Close)
    case MathError(msg, _) => "Error: " + msg
    case MathTrue => format(FmtCode.True)
    case MathFalse => format(FmtCode.False)
    case MathList(values) => format(FmtCode.StartList) + values.map(this.formatNoTrunc).mkString(format(FmtCode.ElementSep)) + format(FmtCode.EndList)
    case MathMatrix(values) => format(FmtCode.StartMatrix) + values.map(col => col.map(this.formatNoTrunc).mkString(format(FmtCode.ElementSep))).mkString(format(FmtCode.GroupSep)) + format(FmtCode.EndMatrix)
    case MathNumber(num) => formatComplex(num)
    case MathPolynomial(coefficients) => formatPol(coefficients)
    case func: MathFunction => func.string(this)
  }
  
  def format(code: FmtCode): String = lexer.format(code)
  def escapeErrorLiteral(msg: String): String = lexer.escapeErrorLiteral(msg)
  
  def format(value: MathValue): String = {
    val str: String = formatNoTrunc(value)
    properties(CalculatorProperties.Truncate) match {
      case 0 => str
      case max if str.length <= max => str
      case max => str.substring(0, max) + " " + format(FmtCode.Truncate)
    }
  }
  
  def parseNumber(integral: String, fraction: Option[String], exponent: Option[String]): Result[MathValue] = {
    exponent match {
      case Some(exp) => exp.toDoubleOption match {
        case Some(expNum) if !expNum.isValidInt => return Result.Error("Exponent out of range: " + exp)
        case _ =>
      }
      case None =>
    }
    try {
      val str = integral + fraction.map("." + _).getOrElse("") + exponent.map("E" + _).getOrElse("")
      Result.Value(MathNumber(BigDecimal(BigDecimalMath.toBigDecimal(str, mathContext))))
    } catch {
      case e: NumberFormatException => Result.Error("Number expected. Are your catcodes screwed up? " + e.getMessage)
    }
  }
  
  def parse(rawLine: String): Result[MathValue] = {
    if (!ready) throw new IllegalStateException("Calculator not ready.")
    if (dumpedUnusable) throw new IllegalStateException("Can't use calculator after dump.")
    
    val normalizedLine: String = properties(CalculatorProperties.Normalization) match {
      case Some(normalization) => Normalizer.normalize(rawLine, normalization)
      case None => rawLine
    }
    
    def compute(tokens: TokenStream): Result[MathValue] = parser.expression(tokens) ~ computeAst
    def computeAst(expr: Ast.Expression): MathValue = {
      val bound = BindLogic.bind(expr, this, eager = properties(CalculatorProperties.Eager))
      ComputationLogic.compute(bound, this)
    }
    
    val result: Result[MathValue] = try {
      normalizedLine match {
        case commands.Let(cmdStr) => lexer.tokenizeAssignment(cmdStr) ~> {
          case PartialTokenStream(tokens, remaining) => parser.letCommand(tokens) ~> {
            case Ast.LetCommand(name: String) => lexer.continue(remaining) ~> compute ~ (value => resolution.let(name, value))
          }
        }
        case commands.Def(cmdStr) => lexer.tokenizeAssignment(cmdStr) ~> {
          case PartialTokenStream(tokens, remaining) => parser.defCommand(tokens) ~> {
            case Ast.DefCommand(target, priority, sig) => lexer.continue(remaining) ~> parser.expression ~> (astExpr => {
              if (!target.isInstanceOf[Ast.DefTarget.Operator] && !target.isInstanceOf[Ast.DefTarget.SignOrOperator] && priority.isDefined) {
                Result.Error("Only binary operators can set a priority")
              } else {
                val thePriority: Either[Int, MathError] = priority match {
                  case Some(priorityExpr) => computeAst(priorityExpr).number(this) match {
                    case MathRealNumeric(real) if real.isWhole => Left(real.toInt)
                    case err: MathError => Right(err)
                    case other => Right(MathError("invalid priority: " + format(other)))
                  }
                  case None => Left(0)
                }
                thePriority match {
                  case Right(err) => Result.Value(err)
                  case Left(thePriorityNum) => resolution.define(target, sig, astExpr) match {
                    case err: MathError => Result.Value(err)
                    case res if priority.isDefined =>
                      resolution.priority(target.name, thePriorityNum)
                      Result.Value(res)
                    case res => Result.Value(res)
                  }
                }
              }
            })
          }
        }
        case commands.Rem(cmdStr) => lexer.continue(cmdStr) ~> parser.remCommand ~ {
          case Ast.RemCommand(target) => resolution.remove(target)
        }
        case commands.Set(cmdStr) => cmdStr match {
          case commands.Fmt(fmtCmdStr) => lexer.splitAssignment(fmtCmdStr) ~> {
            case SplitText(before, after) => FmtCode.byName(before.string) match {
              case Some(fmtCode) => lexer.continue(after) ~> parser.errorToken match {
                case Result.Value(formatString) => lexer.fmtCode(fmtCode, formatString); Result.Value(MathVoid)
                case err @ Result.Error(_, _) => err
              }
              case None => Result.Error("Unknown format code: '" + before.string.strip() + "'")
            }
          }
          case _ => lexer.tokenizeAssignment(cmdStr) ~> {
            case PartialTokenStream(tokens, remaining) => parser.setCommand(tokens) ~> {
              case Ast.SetCommand(name: String) => lexer.continue(remaining) ~> compute ~ (value => properties.set(name, value))
            }
          }
        }
        case commands.Cat(cmdStr) => lexer.tokenizeAssignment(cmdStr) ~> {
          case PartialTokenStream(tokens, remaining) => parser.catCommand(tokens) ~> {
            case Ast.CatCommand(codePoint) => CatCode.byName(remaining.string) match {
              case Some(catCode) => lexer.catCode(codePoint, catCode); Result.Value(MathVoid)
              case None => Result.Error("Unknown catcode: '" + remaining.string.strip() + "'")
            }
          }
        }
        case commands.Tok(cmdStr) => lexer.tokenizeAssignment(cmdStr) ~> {
          case PartialTokenStream(tokens, remaining) => parser.tokCommand(tokens) ~> {
            case Ast.TokCommand(token) => CatCode.byName(remaining.string) match {
              case Some(catCode) => lexer.tokCode(token, catCode); Result.Value(MathVoid)
              case None => Result.Error("Unknown catcode: '" + remaining.string.strip() + "'")
            }
          }
        }
        case commands.Dump(cmdStr) if ini => lexer.continue(cmdStr) ~> parser.dumpCommand ~ {
          case Ast.DumpCommand(fileName) =>
            val out = new DataOutputStream(frontend.openFile(fileName + ".tuxf"))
            try {
              FormatIO.dump(this, out)
            } catch {
              case e: Exception => frontend.showError(e.getClass.getSimpleName + ": " + e.getMessage)
            } finally {
              try {
                out.close()
              } catch {
                case _: Exception =>
              }
            }
            dumpedUnusable = true
            frontend.exit()
            // In case exit does not work.
            return Result.Value(MathVoid)
        }
        case _ => lexer.tokenize(normalizedLine) match {
          case Result.Value(TokenStream(Vector())) =>
            // No tokens, return void result without changing answer
            return Result.Value(MathVoid)
          case tokenResult => tokenResult ~> compute
        }
      }
    } catch {
      case _: StackOverflowError => Result.Error("stack overflow")
      case e: ArithmeticException => Result.Error("error: " + e.getMessage)
    }
    result match {
      case Result.Value(value) if !value.isInstanceOf[MathError] => _answer = value
      case _ =>
    }
    result
  }
}
