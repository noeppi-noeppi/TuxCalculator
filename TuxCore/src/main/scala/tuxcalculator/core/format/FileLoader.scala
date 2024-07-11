package tuxcalculator.core.format

import tuxcalculator.core.Calculator
import tuxcalculator.core.util.Result
import tuxcalculator.core.value.MathError

import java.io.{BufferedReader, Reader}
import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer

object FileLoader {
  
  def load(calc: Calculator, path: Path): Seq[Result.Error] = {
    FileLoader.load(calc, path.toAbsolutePath.normalize.getFileName.toString, Files.lines(path).toArray.toVector.map(_.toString))
  }
  
  def load(calc: Calculator, fileName: String, reader: Reader): Seq[Result.Error] = {
    try {
      val bufferedReader = new BufferedReader(reader)
      FileLoader.load(calc, fileName, bufferedReader.lines.toArray.toVector.map(_.toString))
    } finally {
      reader.close()
    }
  }
  
  def load(calc: Calculator, fileName: String, lines: Vector[String]): Seq[Result.Error] = {
    val errors = ListBuffer[Result.Error]()
    for ((line, lineNum) <- lines.zipWithIndex) calc.parse(line) match {
      case Result.Error(err, trace) => errors.addOne(Result.Error("In " + fileName + ":" + (lineNum + 1) + ": " + err.strip(), trace))
      case Result.Value(MathError(err, trace)) => errors.addOne(Result.Error("In " + fileName + ":" + (lineNum + 1) + ": " + err.strip(), trace))
      case _ =>
    }
    errors.toSeq
  }
}
