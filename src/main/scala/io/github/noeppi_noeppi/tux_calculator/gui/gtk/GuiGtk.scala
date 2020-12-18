package io.github.noeppi_noeppi.tux_calculator.gui.gtk

import io.github.noeppi_noeppi.tux_calculator.Main

import java.io.{IOException, InputStreamReader, Reader}
import java.text.ParseException
import io.github.noeppi_noeppi.tux_calculator.gui.GuiType
import io.github.noeppi_noeppi.tux_calculator.math.parser.Parser
import org.gnome.gtk.{Box, Builder, Button, Gtk, ScrolledWindow, TextView, Widget, Window}

object GuiGtk extends GuiType {

  private lazy val windowData: String = {
    this.getClass.getResourceAsStream("window.xml")
    var reader: Reader = null
    try {
      reader = new InputStreamReader(this.getClass.getResourceAsStream("window.xml"))
    } catch {
      case _: NullPointerException =>
        System.err.println("Could not load main window: " + "glade-xml not found.")
        System.exit(-1)
    }
    val chr = new Array[Char](8 * 1024)
    val buffer = new StringBuilder
    var numCharsRead = 0
    while (numCharsRead >= 0) {
      buffer.appendAll(chr, 0, numCharsRead)
      numCharsRead = reader.read(chr)
    }
    reader.close()
    buffer.toString
  }

  override def display(parser: Parser): Unit = {
    if (!Gtk.isInitialized) Gtk.init(Array())

    val builder = new Builder

    try {
      builder.addFromString(windowData)
    } catch {
      case e@(_: IOException | _: ParseException) =>
        System.err.println("Could not create main window: " + e.getClass.getSimpleName)
        e.printStackTrace()
        System.exit(-1)
    }

    val window = builder.getObject("tux_window").asInstanceOf[Window]

    window.connect(new Widget.Hide {
      override def onHide(widget: Widget): Unit = {
        Gtk.mainQuit(); System.exit(0)
      }
    })

    val tm = new TermManager(parser,
      builder.getObject("tux_term_input").asInstanceOf[TextView],
      builder.getObject("tux_term_output").asInstanceOf[TextView],
      builder.getObject("tux_term_enter").asInstanceOf[Button],
      builder.getObject("tux_term_scroll").asInstanceOf[ScrolledWindow])

    val nm = new NumberManager(builder.getObject("tux_numstr_input").asInstanceOf[TextView],
      builder.getObject("tux_numstr_output").asInstanceOf[TextView])

    DocumentationManager.buildDoc(builder.getObject("tux_doc_root").asInstanceOf[Box], parser.docElems)

    builder.getObject("tux_term_input").asInstanceOf[TextView].grabFocus()

    window.showAll()
    Gtk.main()
  }
}
