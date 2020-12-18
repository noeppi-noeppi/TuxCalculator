package io.github.noeppi_noeppi.tux_calculator.gui.gtk

import io.github.noeppi_noeppi.tux_calculator.NumberStringer
import io.github.noeppi_noeppi.tux_calculator.math.AUtil
import io.github.noeppi_noeppi.tux_calculator.math.parser.{FuncData, Parser}
import org.gnome.gdk.{EventKey, Keyval, Rectangle}
import org.gnome.gtk.{Button, Justification, ScrolledWindow, TextBuffer, TextTag, TextTagTable, TextView, Widget}
import org.gnome.pango.{Scale, Weight}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TermManager(private val parser: Parser, val in: TextView, private val out: TextView, private val enter: Button, private val scrollPane: ScrolledWindow) {

  private var last = ""
  private var arrowIdx = -1
  private var current = ""
  private val historyBuffer = ListBuffer[String]()

  private val outTable = new TextTagTable()
  out.setBuffer(new TextBuffer(outTable))

  private val inTable = new TextTagTable()
  private val inTag: TextTag = new TextTag(inTable)
  inTag.setScale(Scale.X_LARGE)
  in.setBuffer(new TextBuffer(inTable))

  private val resultTags = mutable.Set[(Int, Int)]()

  val outTag: TextTag = new TextTag(outTable)
  outTag.setJustify(Justification.RIGHT)
  outTag.setWeight(Weight.BOLD)

  private var tabList: List[String] = _
  private var tabIdx = -1
  private var lastTabCurserPos = -1

  in.connect(new Widget.KeyPressEvent {
    override def onKeyPressEvent(widget: Widget, eventKey: EventKey): Boolean =
      if (eventKey.getKeyval == Keyval.Return || eventKey.getKeyval.toString.toLowerCase().contains("kp_enter")) {
        delTab()
        calcTerm()
        true
      } else if (eventKey.getKeyval == Keyval.Up || eventKey.getKeyval.toString.toLowerCase().contains("kp_up")) {
        delTab()
        incrHist()
        true
      } else if (eventKey.getKeyval == Keyval.Down || eventKey.getKeyval.toString.toLowerCase().contains("kp_down")) {
        delTab()
        decrHist()
        true
      } else if (eventKey.getKeyval == Keyval.Tab) {
        updateTab(false)
        true
      } else if (eventKey.getKeyval == Keyval.BackTab) {
        updateTab(true)
        true
      } else {
        if (eventKey.getKeyval.toUnicode != 0)
          delTab()
        false
      }
  })
  in.getBuffer
  enter.connect(new Button.Clicked {
    override def onClicked(button: Button): Unit = calcTerm()
  })
  out.connect(new Widget.SizeAllocate {
    override def onSizeAllocate(widget: Widget, rectangle: Rectangle): Unit = scrollPane.getVAdjustment.setValue(scrollPane.getVAdjustment.getUpper)
  })

  private def incrHist(): Unit = {
    if (arrowIdx < 0) {
      current = in.getBuffer.getText
    }
    arrowIdx += 1
    if (arrowIdx >= historyBuffer.length)
      arrowIdx = historyBuffer.length - 1
    if (arrowIdx < 0)
      in.getBuffer.setText(current)
    else
      in.getBuffer.setText(historyBuffer(arrowIdx))
  }

  private def decrHist(): Unit = {
    if (arrowIdx >= 0) {
      arrowIdx -= 1
    }
    if (arrowIdx < 0)
      in.getBuffer.setText(current)
    else
      in.getBuffer.setText(historyBuffer(arrowIdx))
  }

  private def calcTerm(): Unit = {
    var term = in.getBuffer.getText.trim
    in.getBuffer.setText("")

    if (term.trim.isEmpty)
      term = last.trim
    else
      last = term

    if (term.isEmpty) // If last was empty as well
      return

    if (historyBuffer.isEmpty || term != historyBuffer.head) {
      historyBuffer.prepend(term)
    }
    arrowIdx = -1

    var mode = DisplayMode.NUMBER
    if (term.startsWith("number?") || term.startsWith("n?")) {
      term = term.substring(term.indexOf('?') + 1).trim
      mode = DisplayMode.NUMBER
    } else if (term.startsWith("text?") || term.startsWith("t?")) {
      term = term.substring(term.indexOf('?') + 1).trim
      mode = DisplayMode.TEXT
    } else if (term.startsWith("function?") || term.startsWith("func?") || term.startsWith("f?")) {
      term = term.substring(term.indexOf('?') + 1).trim
      mode = DisplayMode.FUNCTION
    } else if (term.startsWith("list?") || term.startsWith("l?")) {
      term = term.substring(term.indexOf('?') + 1).trim
      mode = DisplayMode.LIST
    }
    if (term.isEmpty && !last.isEmpty)
      term = "ans"

    var result = ""
    try {
      result = toStringWithMode(parser.parse(term), mode, parser.funcData)
    } catch {
      case e: Exception => result = "Fehler: " + e.getMessage
    }
    out.getBuffer.setText(out.getBuffer.getText + "\n" + term)
    val start = out.getBuffer.getCharCount
    out.getBuffer.setText(out.getBuffer.getText + "\n" + result)
    val end = out.getBuffer.getCharCount
    resultTags += ((start, end))
    updateResultTags()
    scrollPane.getVAdjustment.setValue(scrollPane.getVAdjustment.getUpper)
    in.grabFocus()
  }

  private def updateResultTags(): Unit = {
    for (x <- resultTags) {
      out.getBuffer.applyTag(outTag, out.getBuffer.getIter(x._1), out.getBuffer.getIter(x._2))
    }
  }

  private def updateTab(inverted: Boolean): Unit = {
    if (in.getBuffer.getHasSelection || (lastTabCurserPos >= 0 && lastTabCurserPos != in.getBuffer.getCursorPosition)) {
      delTab()
      return
    }
    val former = if (tabIdx < 0) {
      val str = in.getBuffer.getText(in.getBuffer.getIterStart, in.getBuffer.getIter(in.getBuffer.getInsert), true)
      val tabbedStr = str.substring(str.lastIndexWhere(c => !c.isLetter && c != '_') + 1)
      tabList = if (str.length > tabbedStr.length && str.charAt(str.length - tabbedStr.length - 1) == '@') {
        if (tabbedStr.length == 0)
          parser.functionTabComplete.filter(p => p.toLowerCase().startsWith(tabbedStr.toLowerCase())).prepended("[]")
        else
          parser.functionTabComplete.filter(p => p.toLowerCase().startsWith(tabbedStr.toLowerCase()))
      } else {
        if (tabbedStr.length == 0)
          parser.allTabComplete.filter(p => p.toLowerCase().startsWith(tabbedStr.toLowerCase())).prepended("()")
        else
          parser.allTabComplete.filter(p => p.toLowerCase().startsWith(tabbedStr.toLowerCase()))
      }
      if (tabList.isEmpty) {
        delTab()
        return
      }
      if (inverted) {
        tabIdx = tabList.size - 1
      } else {
        tabIdx = 0
      }
      tabbedStr
    } else {
      val ret = tabList(tabIdx)
      if (ret.endsWith(")") || ret.endsWith("]"))
        in.getBuffer.placeCursor(in.getBuffer.getIter(in.getBuffer.getCursorPosition + 1))
      if (inverted)
        tabIdx = (tabIdx + tabList.size - 1) % tabList.size
      else
        tabIdx = (tabIdx + 1) % tabList.size
      ret
    }
    val bufferTextBefore = in.getBuffer.getText(in.getBuffer.getIterStart, in.getBuffer.getIter(in.getBuffer.getInsert), true)
    val bufferTextAfter = in.getBuffer.getText(in.getBuffer.getIter(in.getBuffer.getInsert), in.getBuffer.getIterEnd, true)
    in.getBuffer.setText(bufferTextBefore.substring(0, bufferTextBefore.length - former.length) + tabList(tabIdx) + bufferTextAfter)
    if (tabList(tabIdx).endsWith(")") || tabList(tabIdx).endsWith("]"))
      in.getBuffer.placeCursor(in.getBuffer.getIter(bufferTextBefore.length - former.length + tabList(tabIdx).length - 1))
    else
      in.getBuffer.placeCursor(in.getBuffer.getIter(bufferTextBefore.length - former.length + tabList(tabIdx).length))
    lastTabCurserPos = in.getBuffer.getCursorPosition
  }

  private def delTab(): Unit = {
    tabIdx = -1
    tabList = null
    lastTabCurserPos = -1
  }

  private def toStringWithMode(result: Double, mode: DisplayMode, fp: FuncData): String = {
    mode match {
      case DisplayMode.TEXT =>
        if (result.isNaN || result.isInfinite) {
          result.toString
        } else {
          NumberStringer.toString(result.toString)
        }
      case DisplayMode.FUNCTION =>
        if (result.isNaN || result.isInfinite) {
          return "Invalid Pointer: " + result.toString
        }
        val mf = fp.get(result.toInt)
        if (mf == null) {
          return "Invalid Function: " + result.toString
        }
        s"Function(Name:'${mf.name}', Params:${mf.params}, Descriptive:'${mf.name2}')"
      case DisplayMode.LIST =>
        if (result.isNaN || result.isInfinite) {
          return "Invalid Pointer: " + result.toString
        }
        val mf = fp.get(result.toInt)
        if (mf == null) {
          return "Invalid Function: " + result.toString
        }
        val list = AUtil.getList(fp, mf)
        if (list == null) {
          return "Invalid List: " + result.toString
        }
        list.map(d => {
          var str = d.toString
          if (str.endsWith(".0")) {
            str = str.substring(0, str.length - 2)
          }
          str
        }).mkString("[", ", ", "]")
      case _ =>
        var resultStr = result.toString
        if (resultStr.endsWith(".0")) {
          resultStr = resultStr.substring(0, resultStr.length - 2)
        }
        resultStr
    }
  }
}
