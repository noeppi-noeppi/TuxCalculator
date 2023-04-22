package tuxcalculator.core.format

import tuxcalculator.core.Calculator
import tuxcalculator.core.util.Result
import tuxcalculator.core.value.MathError

import java.io.{BufferedReader, Reader}
import java.nio.file.{Files, Path}
import scala.collection.mutable.ListBuffer

object FileLoader {
  
  def load(calc: Calculator, path: Path): Seq[String] = {
    FileLoader.load(calc, path.toAbsolutePath.normalize.getFileName.toString, Files.lines(path).toArray.toVector.map(_.toString))
  }
  
  def load(calc: Calculator, fileName: String, reader: Reader): Seq[String] = {
    try {
      val bufferedReader = new BufferedReader(reader)
      FileLoader.load(calc, fileName, bufferedReader.lines.toArray.toVector.map(_.toString))
    } finally {
      reader.close()
    }
  }
  
  def load(calc: Calculator, fileName: String, lines: Vector[String]): Seq[String] = {
    val errors = ListBuffer[String]()
    for ((line, lineNum) <- lines.zipWithIndex) calc.parse(line) match {
      case Result.Error(err) => errors.addOne("In " + fileName + ":" + (lineNum + 1) + ": " + err)
      case Result.Value(MathError(err, trace)) => errors.addOne("In " + fileName + ":" + (lineNum + 1) + ": " + err + " at (" + trace.mkString(" -> ") + ")")
      case _ =>
    }
    errors.toSeq
  }
}
