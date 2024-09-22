package tuxcalculator.dev

import org.apache.commons.io.file.PathUtils
import tuxcalculator.api.{TuxCalculatorAPI, TuxFrontend}
import tuxcalculator.core.Calculator
import tuxcalculator.core.format.FileLoader

import java.io.OutputStream
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.Objects

object Main {
  
  val source: Path = Paths.get(Objects.requireNonNull(System.getProperty("tuxcalculator.dev.source"), "Source path not given")).toAbsolutePath.normalize
  val target: Path = Paths.get(Objects.requireNonNull(System.getProperty("tuxcalculator.dev.target"), "Target path not given")).toAbsolutePath.normalize

  def main(args: Array[String]): Unit = {
    val version = System.getProperty("tuxcalculator.dev.version")
    if (version == null) throw new IllegalStateException("Dev version not set.")
    if (version != TuxCalculatorAPI.VERSION) throw new IllegalStateException("Project version does not match source version.")
    
    if (Files.isDirectory(target)) PathUtils.deleteDirectory(target)
    
    val calc = new Calculator(DevFrontend, ini = true)
    calc.resolution.produceFrontendErrorOnUnboundValue()
    val errors = FileLoader.load(calc, source.resolve("tuxcalculator/plain.tuxc"))
    if (errors.nonEmpty) {
      System.err.println("There were errors compiling plain.tuxc")
      for (error <- errors) {
        System.err.println("  " + error.msg)
        for (trace <- error.trace) System.err.println("    " + trace)
      }
      System.exit(1)
    }
  }
  
  object DevFrontend extends TuxFrontend {
    override def showError(err: String): Unit = {
      System.err.println(err)
      throw new RuntimeException(err.takeWhile(_ != '\n').strip())
    }
    override def openFile(fileName: String): OutputStream = {
      val outPath = target.resolve("tuxcalculator").resolve(fileName)
      Files.createDirectories(outPath.getParent)
      Files.newOutputStream(outPath, StandardOpenOption.CREATE_NEW, StandardOpenOption.TRUNCATE_EXISTING)
    }
    override def exit(): Unit = ()
  }
}
