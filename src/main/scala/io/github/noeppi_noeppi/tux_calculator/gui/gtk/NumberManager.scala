package io.github.noeppi_noeppi.tux_calculator.gui.gtk

import io.github.noeppi_noeppi.tux_calculator.NumberStringer

import java.util.concurrent.{ExecutorService, ScheduledThreadPoolExecutor}
import org.gnome.gdk.{EventKey, Keyval}
import org.gnome.gtk.{TextBuffer, TextTag, TextTagTable, TextView, Widget}

class NumberManager(val in: TextView, private val out: TextView) {

  val thread: ExecutorService = new ScheduledThreadPoolExecutor(1)

  private val table = new TextTagTable()
  private val tag: TextTag = new TextTag(table)
  tag.setFont("Monospace")
  out.setBuffer(new TextBuffer(table))

  in.connect(new Widget.KeyPressEvent {
    override def onKeyPressEvent(widget: Widget, eventKey: EventKey): Boolean = if (eventKey.getKeyval == Keyval.Return  || eventKey.getKeyval.toString.toLowerCase().contains("kp_enter")) {
      updateNumber()
      true
    } else {
      false
    }
  })

  private def updateNumber(): Unit = {
    thread.execute(() => {
      try {
        out.getBuffer.setText("Bitte Warten ...")
        if (in.getBuffer.getText.trim.equalsIgnoreCase("1e10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000")) {
          out.getBuffer.setText("Googolplex\n\n    ____\n   /    \\\n  /      \\\n | Easter |\n |        |\n |  Egg   |\n  \\______/")
          out.getBuffer.applyTag(tag, out.getBuffer.getIter(12), out.getBuffer.getIterEnd)
        } else {
          val str = getNumString(in.getBuffer.getText().toLowerCase())
          out.getBuffer.setText(NumberStringer.toString(str))
        }
      } catch {
        case _@(_: IllegalArgumentException | _: MatchError | _: NumberFormatException | _: NegativeArraySizeException) => out.getBuffer.setText("Fehler")
      }
    })
  }

  def getNumString(str: String): String = {
    if (str.contains("e")) {
      val bd = BigDecimal(str)
      bd.underlying().toPlainString
    } else {
      str
    }
  }
}
