package io.github.noeppi_noeppi.tux_calculator

import io.github.noeppi_noeppi.tux_calculator.gui.gtk.GuiGtk

import java.io.IOException
import java.nio.file.{Files, Paths}
import java.util.Scanner
import java.util.stream.Collectors
import io.github.noeppi_noeppi.tux_calculator.math.parser.ParserBuilder

import scala.jdk.CollectionConverters._

object Main extends App {

  val GUI = args.length != 1 || !args(0).equalsIgnoreCase("nogui")

  var parser = ParserBuilder().addDefault().build()
  val rcFile = if (System.getProperty("tuxtr.rcfile") != null && System.getProperty("tuxtr.rcfile").nonEmpty) {
    Paths.get(System.getProperty("tuxtr.rcfile"))
  } else {
    Paths.get(System.getProperty("user.home")).resolve(".tuxtr.rc")
  }
  
  if (Files.isRegularFile(rcFile)) {
    try {
      parser = parser.derive(Files.newBufferedReader(rcFile).lines().collect(Collectors.toList()).asScala.toList)
    } catch {
      case e @ (_: IllegalArgumentException | _: MatchError | _:IOException) =>
        println("Konnte RC-Datei nicht lesen: " + e.getMessage)
        e.printStackTrace()
    }
  }

  if (GUI) {
    val guiType = GuiGtk

    guiType.display(parser)
  } else {
    val scanner = new Scanner(System.in)

    while (true) {
      val line = try {
        scanner.nextLine()
      } catch {
        case _: NoSuchElementException =>
          System.exit(0); throw new Error
      }
      if (line.nonEmpty) {
        try {
          val str = parser.parse(line).toString
          if (str.endsWith(".0"))
            println(str.substring(0, str.length - 2))
          else
            println(str)
        } catch {
          case e: Exception =>
            println("Fehler: " + e.getMessage)
        }
      } else {
        println()
      }
    }
  }
}
