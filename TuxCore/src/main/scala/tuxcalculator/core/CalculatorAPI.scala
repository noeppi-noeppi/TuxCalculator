package tuxcalculator.core

import tuxcalculator.api.TuxCalculator.HighlightPart
import tuxcalculator.api.{TuxCalculator, TuxCalculatorAPI, TuxFrontend}
import tuxcalculator.core.format.{FileLoader, FormatIO, InvalidFormatException}
import tuxcalculator.core.util.{InputHighlighter, Result, TabCompleter, Util}
import tuxcalculator.core.value.{MathError, MathVoid}

import java.io._
import java.nio.file.{Files, Path}
import java.util
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object CalculatorAPI extends TuxCalculatorAPI {

  override def createINI(frontend: TuxFrontend): TuxCalculator = new CalculatorWrapper(new Calculator(frontend, ini = true))
  
  override def createPlain(frontend: TuxFrontend): TuxCalculator.Builder = {
    val in = classOf[Calculator].getResourceAsStream("/tuxcalculator/plain.tuxf")
    if (in == null) new ErroredCalculatorBuilder(Vector(Result.Error("plain format file not found.")))
    else createBy(frontend, in)
  }
  
  override def createBy(frontend: TuxFrontend, fmt: Path): TuxCalculator.Builder = {
    if (Files.isRegularFile(fmt)) {
      createBy(frontend, Files.newInputStream(fmt))
    } else {
      new ErroredCalculatorBuilder(Vector(Result.Error("Format file not found: " + fmt) ~@ fmt.toAbsolutePath.normalize().toString))
    }
  }
  
  override def createBy(frontend: TuxFrontend, fmt: Array[Byte]): TuxCalculator.Builder = createBy(frontend, new ByteArrayInputStream(fmt))
  
  override def createBy(frontend: TuxFrontend, fmt: InputStream): TuxCalculator.Builder = {
    try {
      val in = new DataInputStream(fmt)
      new CalculatorBuilderWrapper(FormatIO.load(frontend, in))
    } catch {
      case e: EOFException => new ErroredCalculatorBuilder(Vector(Result.Error("Truncated format file.")) ++ Option(e.getMessage).map(msg => Result.Error(msg)).toVector)
      case e: UTFDataFormatException => new ErroredCalculatorBuilder(Vector(Result.Error("Charset error.")) ++ Option(e.getMessage).map(msg => Result.Error(msg)).toVector)
      case e: IOException => new ErroredCalculatorBuilder(Vector(Result.Error("Generic IO error.")) ++ Option(e.getMessage).map(msg => Result.Error(msg)).toVector)
      case e: InvalidFormatException => new ErroredCalculatorBuilder(Vector(Result.Error("Corrupted format file.")) ++ Option(e.getMessage).map(msg => Result.Error(msg)).toVector)
      case e: Exception => new ErroredCalculatorBuilder(Vector(Result.Error("Error while loading format file.", Util.getStacktrace(e))))
    } finally {
      fmt.close()
    }
  }
  
  private class CalculatorBuilderWrapper(private val calc: Calculator) extends TuxCalculator.Builder {
    
    private val errors: mutable.IndexedBuffer[Result.Error] = mutable.ArrayBuffer()
    
    override def load(path: Path): Unit = {
      if (Files.isRegularFile(path)) {
        errors.addAll(FileLoader.load(calc, path))
      } else {
        errors.addOne(Result.Error("File not found: " + path) ~@ path.toAbsolutePath.normalize().toString)
      }
    }
    
    override def load(fileName: String, in: InputStream): Unit = load(fileName, new InputStreamReader(in))
    override def load(fileName: String, in: Reader): Unit = errors.addAll(FileLoader.load(calc, fileName, in))
    override def checkError(): util.List[TuxCalculator.Error] = if (errors.isEmpty) null else errors.map {
      case Result.Error(msg, trace) => new TuxCalculator.Error(msg, trace.asJava)
    }.to(ArraySeq).asJava
    override def build(): TuxCalculator = {
      if (errors.isEmpty) new CalculatorWrapper(calc)
      else throw new IllegalStateException("There were errors building the calculator.")
    }
  }
  
  private class ErroredCalculatorBuilder(val errors: Seq[Result.Error]) extends TuxCalculator.Builder {
    override def load(path: Path): Unit = ()
    override def load(fileName: String, in: InputStream): Unit = ()
    override def load(fileName: String, in: Reader): Unit = ()
    override def checkError(): util.List[TuxCalculator.Error] = errors.map {
      case Result.Error(msg, trace) => new TuxCalculator.Error(msg, trace.asJava)
    }.to(ArraySeq).asJava
    override def build(): TuxCalculator = throw new IllegalStateException("There were errors building the calculator.")
  }
  
  private class CalculatorWrapper(val calc: Calculator) extends TuxCalculator {
    override def ini(): Boolean = calc.ini
    override def highlight(line: String): util.List[HighlightPart] = InputHighlighter.highlight(calc, line).asJava
    override def tabComplete(line: String): TuxCalculator.TabCompletion = TabCompleter.tabComplete(calc, line) match {
      case TabCompleter.Result(prefix, completionString, matches, isIdentifier) => new TuxCalculator.TabCompletion(prefix, completionString, matches.asJava, isIdentifier)
    }
    override def parse(line: String): TuxCalculator.Result = calc.parse(line) match {
      case Result.Value(MathVoid) => new TuxCalculator.Void()
      case Result.Value(MathError(msg, trace)) => new TuxCalculator.Error(msg, trace.asJava)
      case Result.Error(msg, trace) => new TuxCalculator.Error(msg, trace.asJava)
      case Result.Value(value) => new TuxCalculator.Success(calc.format(value))
    }
  }
}
