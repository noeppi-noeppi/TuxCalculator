package tuxcalculator.core

import ch.obermuhlner.math.big.BigDecimalMath
import tuxcalculator.api.TuxFrontend
import tuxcalculator.core.data.{CalculatorCommands, CalculatorProperties, CalculatorSpecials, PropertyAccess}
import tuxcalculator.core.expression.Ast
import tuxcalculator.core.format.FormatIO
import tuxcalculator.core.lexer.{CatCode, Lexer, PartialTokenStream, TokenStream}
import tuxcalculator.core.parser.{Parser, ParsingContext}
import tuxcalculator.core.resolution.{BindLogic, ComputationLogic, ResolutionTable}
import tuxcalculator.core.util.{Result, Util}
import tuxcalculator.core.value._

import java.io.DataOutputStream
import java.math.{MathContext, RoundingMode}

class Calculator(val frontend: TuxFrontend, val ini: Boolean) extends ParsingContext with PropertyAccess {

  private[this] var ready: Boolean = ini
  private[this] var dumpedUnusable: Boolean = false
  
  val lexer = new Lexer
  val parser = new Parser(this)
  
  val properties: CalculatorProperties = new CalculatorProperties(this, () => {
    specials.propertyChange()
    _mathContext = null
    _outputMathContext = null
  })
  val specials: CalculatorSpecials = new CalculatorSpecials(this)
  val resolution: ResolutionTable = new ResolutionTable(this)
  private val commands: CalculatorCommands = new CalculatorCommands(lexer)

  private[this] var _mathContext: MathContext = _
  private[this] var _outputMathContext: MathContext = _
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
      _outputMathContext = new MathContext(precision min properties(CalculatorProperties.Output), RoundingMode.HALF_UP)
    }
    _outputMathContext
  }

  def answer: MathValue = _answer
  def finish(answer: MathValue): Unit = if (ready) throw new IllegalStateException("Calculator already properly loaded.") else {
    _answer = answer
    ready = true
  }
  
  private def formatNum(value: BigDecimal): String = value.round(outputMathContext) match {
    case v if v.abs < 1000000 && v.abs >= 0.0001 => Util.safeStripTrailingZeros(v).toPlainString
    case v if v.isWhole && v.abs < 100000000 => Util.safeStripTrailingZeros(v).toPlainString
    case v => Util.formatScientific(Util.safeStripTrailingZeros(v))
  }
  
  private def formatNoTrunc(value: MathValue): String = value match {
    case MathVoid => "()"
    case MathError(msg, _) => "Error: " + msg
    case MathTrue => "true"
    case MathFalse => "false"
    case MathList(values) => "[" + values.map(this.formatNoTrunc).mkString(", ") + "]"
    case MathMatrix(values) => "#[" + values.map(col => col.map(this.formatNoTrunc).mkString(",")).mkString(" ; ") + "]"
    case MathRealNumeric(real) => formatNum(real)
    case MathNumber(num) if num.im.signum() == -1 => formatNum(num.re) + " - " + formatNum(num.im.negate()) + "i"
    case MathNumber(num) => formatNum(num.re) + " + " + formatNum(num.im) + "i"
    case func: MathFunction => func.string(this)
  }
  
  def format(value: MathValue): String = {
    val str: String = formatNoTrunc(value)
    properties(CalculatorProperties.Truncate) match {
      case 0 => str
      case max if str.length <= max => str
      case max => str.substring(0, max) + " [...]"
    }
  }
  
  def parseNumber(integral: String, fraction: Option[String], exponent: Option[String]): Result[MathValue] = try {
    val str = integral + fraction.map("." + _).getOrElse("") + exponent.map("E" + _).getOrElse("")
    Result.Value(MathNumber(BigDecimal(BigDecimalMath.toBigDecimal(str, mathContext))))
  } catch {
    case e: NumberFormatException => Result.Error(e.getMessage)
  }
  
  def parse(line: String): Result[MathValue] = {
    if (!ready) throw new IllegalStateException("Calculator not ready.")
    if (dumpedUnusable) throw new IllegalStateException("Can't use calculator after dump.")
    
    def computeString(string: String): Result[MathValue] = lexer.tokenize(string) ~> computeTokens
    def computeTokens(tokens: TokenStream): Result[MathValue] = parser.expression(tokens) ~ computeAst
    def computeAst(expr: Ast.Expression): MathValue = {
      val bound = BindLogic.bind(expr, this, eager = properties(CalculatorProperties.Eager))
      ComputationLogic.compute(bound, this)
    }
    
    val result: Result[MathValue] = try {
      line.strip() match {
        case commands.Let(cmdStr) => lexer.tokenizeAssignment(cmdStr) ~> {
          case PartialTokenStream(tokens, remaining) => parser.letCommand(tokens) ~> {
            case Ast.LetCommand(name: String) => computeString(remaining) ~ (value => resolution.let(name, value))
          }
        }
        case commands.Def(cmdStr) => lexer.tokenizeAssignment(cmdStr) ~> {
          case PartialTokenStream(tokens, remaining) => parser.defCommand(tokens) ~> {
            case Ast.DefCommand(target, priority, sig) => lexer.tokenize(remaining) ~> parser.expression ~> (astExpr => {
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
        case commands.Rem(cmdStr) => lexer.tokenize(cmdStr) ~> parser.remCommand ~ {
          case Ast.RemCommand(target) => resolution.remove(target)
        }
        case commands.Set(cmdStr) => lexer.tokenizeAssignment(cmdStr) ~> {
          case PartialTokenStream(tokens, remaining) => parser.setCommand(tokens) ~> {
            case Ast.SetCommand(name: String) => computeString(remaining) ~ (value => properties.set(name, value))
          }
        }
        case commands.Cat(cmdStr) => lexer.tokenizeAssignment(cmdStr) ~> {
          case PartialTokenStream(tokens, remaining) => parser.catCommand(tokens) ~> {
            case Ast.CatCommand(codePoint) => CatCode.byName(remaining) match {
              case Some(catCode) => lexer.catCode(codePoint, catCode); Result.Value(MathVoid)
              case None => Result.Error("Unknown catcode: '" + remaining.strip() + "'")
            }
          }
        }
        case commands.Tok(cmdStr) => lexer.tokenizeAssignment(cmdStr) ~> {
          case PartialTokenStream(tokens, remaining) => parser.tokCommand(tokens) ~> {
            case Ast.TokCommand(token) => CatCode.byName(remaining) match {
              case Some(catCode) => lexer.tokCode(token, catCode); Result.Value(MathVoid)
              case None => Result.Error("Unknown catcode: '" + remaining.strip() + "'")
            }
          }
        }
        case commands.Dump(cmdStr) if ini => lexer.tokenize(cmdStr) ~> parser.dumpCommand ~ {
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
        case _ => lexer.tokenize(line) match {
          case Result.Value(TokenStream(Vector())) =>
            // No tokens, return void result without changing answer
            return Result.Value(MathVoid)
          case tokenResult => tokenResult ~> computeTokens
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
