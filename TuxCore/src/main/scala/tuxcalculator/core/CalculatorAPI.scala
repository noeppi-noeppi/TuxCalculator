package tuxcalculator.core

import tuxcalculator.api.{TuxCalculator, TuxCalculatorAPI, TuxFrontend}
import tuxcalculator.core.format.{FileLoader, FormatIO, InvalidFormatException}
import tuxcalculator.core.util.{InputHighlighter, Result, TabCompleter, Util}
import tuxcalculator.core.value.{MathError, MathVoid}

import java.io._
import java.nio.file.{Files, Path}
import java.util
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object CalculatorAPI extends TuxCalculatorAPI {

  override def createINI(frontend: TuxFrontend): TuxCalculator = new CalculatorWrapper(new Calculator(frontend, ini = true))
  
  override def createPlain(frontend: TuxFrontend): TuxCalculator.Builder = {
    val in = classOf[Calculator].getResourceAsStream("/tuxcalculator/plain.tuxf")
    if (in == null) new ErroredCalculatorBuilder(Vector("plain format file not found."))
    else createBy(frontend, in)
  }
  
  override def createBy(frontend: TuxFrontend, fmt: Path): TuxCalculator.Builder = {
    if (Files.isRegularFile(fmt)) {
      createBy(frontend, Files.newInputStream(fmt))
    } else {
      new ErroredCalculatorBuilder(Vector("Format file not found: " + fmt))
    }
  }
  
  override def createBy(frontend: TuxFrontend, fmt: Array[Byte]): TuxCalculator.Builder = createBy(frontend, new ByteArrayInputStream(fmt))
  
  override def createBy(frontend: TuxFrontend, fmt: InputStream): TuxCalculator.Builder = {
    try {
      val in = new DataInputStream(fmt)
      new CalculatorBuilderWrapper(FormatIO.load(frontend, in))
    } catch {
      case e: IOException => new ErroredCalculatorBuilder(Vector("IO error: " + e.getMessage))
      case e: InvalidFormatException => new ErroredCalculatorBuilder(Vector(e.getMessage))
      case e: Exception => new ErroredCalculatorBuilder(Vector("Error while loading format file.") ++ Util.getStacktrace(e))
    } finally {
      fmt.close()
    }
  }
  
  class CalculatorBuilderWrapper(private val calc: Calculator) extends TuxCalculator.Builder {
    
    private val errors: ListBuffer[String] = ListBuffer()
    
    override def load(path: Path): Unit = {
      if (Files.isRegularFile(path)) {
        errors.addAll(FileLoader.load(calc, path))
      } else {
        errors.addOne("File not found: " + path.toAbsolutePath.normalize().toString)
      }
    }
    
    override def load(fileName: String, in: InputStream): Unit = load(fileName, new InputStreamReader(in))
    override def load(fileName: String, in: Reader): Unit = errors.addAll(FileLoader.load(calc, fileName, in))
    override def checkError(): util.List[String] = if (errors.isEmpty) null else errors.toVector.asJava
    override def build(): TuxCalculator = {
      if (errors.isEmpty) new CalculatorWrapper(calc)
      else throw new IllegalStateException("There were errors building the calculator.")
    }
  }
  
  class ErroredCalculatorBuilder(val errors: Vector[String]) extends TuxCalculator.Builder {
    override def load(path: Path): Unit = ()
    override def load(fileName: String, in: InputStream): Unit = ()
    override def load(fileName: String, in: Reader): Unit = ()
    override def checkError(): util.List[String] = errors.asJava
    override def build(): TuxCalculator = throw new IllegalStateException("There were errors building the calculator.")
  }
  
  class CalculatorWrapper(val calc: Calculator) extends TuxCalculator {
    override def ini(): Boolean = calc.ini
    override def highlight(line: String): util.List[TuxCalculator.InputHighlight] = InputHighlighter.highlight(calc, line).asJava
    override def tabComplete(line: String): TuxCalculator.TabCompletion = TabCompleter.tabComplete(calc, line) match {
      case TabCompleter.Result(prefix, completionString, matches, isIdentifier) => new TuxCalculator.TabCompletion(prefix, completionString, matches.asJava, isIdentifier)
    }
    override def parse(line: String): TuxCalculator.Result = calc.parse(line) match {
      case Result.Value(MathVoid) => new TuxCalculator.Void()
      case Result.Value(MathError(msg, trace)) => new TuxCalculator.Error(msg, trace.asJava)
      case Result.Error(msg) => new TuxCalculator.Error(msg, Nil.asJava)
      case Result.Value(value) => new TuxCalculator.Success(calc.format(value))
    }
  }
}
