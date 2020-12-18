package io.github.noeppi_noeppi.tux_calculator.gui.gtk

import io.github.noeppi_noeppi.tux_calculator.math.parser.Parser
import io.github.noeppi_noeppi.tux_calculator.math.{Constant, DocType, DocumentationObject, MFunction, Operator}
import org.gnome.gtk.{Align, Box, Justification, Label, Widget}

import scala.collection.mutable.ListBuffer

object DocumentationManager {

  def buildDoc(box: Box, objs: Set[DocumentationObject]): Unit = {
    for (c <- box.getChildren) {
      box.remove(c)
    }

    val dd = List.from(objs)
    val lb = ListBuffer[Widget]()

    lb += defaultDocWidget("Tux-Taschenrechner", null, "Made by noeppi_noeppi")

    for (x <- dd.filter(d => d.dtype == DocType.GENERAL).sorted) {
      lb += defaultDocWidget(quoteText(x.name) + "   " + x.name2, "Typ: " + DocType.GENERAL.rep, x.doc)
    }
    for (x <- dd.filter(d => d.dtype == DocType.OPERATOR).sorted) {
      lb += defaultDocWidget(quoteText(x.name) + "   " + x.name2, "Typ: " + DocType.OPERATOR.rep + ", Priorität: " + x.asInstanceOf[Operator].priority.rep, x.doc)
    }
    for (x <- dd.filter(d => d.dtype == DocType.UNARY).sorted) {
      lb += defaultDocWidget(quoteText(x.name) + "   " + x.name2, "Typ: " + DocType.UNARY.rep, x.doc)
    }
    for (x <- dd.filter(d => d.dtype == DocType.POSTFIX).sorted) {
      lb += defaultDocWidget(quoteText(x.name) + "   " + x.name2, "Typ: " + DocType.POSTFIX.rep, x.doc)
    }
    for (x <- dd.filter(d => d.dtype == DocType.CONSTANT).sorted) {
      lb += defaultDocWidget(quoteText(x.name) + "   " + x.name2, "Typ: " + DocType.CONSTANT.rep + ", Wert: " + x.asInstanceOf[Constant].value, x.doc)
    }
    for (x <- dd.filter(d => d.dtype == DocType.FUNCTION).sorted) {
      lb += defaultDocWidget(quoteText(x.name) + "   " + x.name2, "Typ: " + DocType.FUNCTION.rep + ", " + getFunctionArgs(x.asInstanceOf[MFunction]), x.doc)
    }

    for (widget <- lb) {
      box.add(widget)
    }
  }

  def defaultDocWidget(headline: String, params: String, desc: String): Widget = {
    val label = new Label()

    label.setJustify(Justification.LEFT)
    label.setAlignHorizontal(Align.START)
    label.setPadding(5, 5)
    label.setLineWrap(true)
    label.setSelectable(true)
    label.setUseMarkup(true)

    val sb = new StringBuilder
    if (headline != null && headline.nonEmpty)
      sb ++= "<span size=\"large\" weight=\"ultrabold\">" ++= headline ++= "</span>\n"
    if (params != null && params.nonEmpty)
      sb ++= "<span weight=\"bold\">" ++= params ++= "</span>\n"
    if (desc != null && desc.nonEmpty)
      sb ++= desc ++= "\n"
    if (sb.nonEmpty)
      sb ++= "\n"

    label.setLabel(sb.toString())

    label
  }

  def getFunctionArgs(f: MFunction): String = f.params match {
    case Parser.VARARG => "Vararg-Funktion (min. 1 Argument)"
    case 0 => "Keine Argumente"
    case 1 => "Ein Argument"
    case p => p.toString + " Argumente"
  }

  def quoteText(text: String): String = text
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")

  implicit val DocObjectOrdering: Ordering[DocumentationObject] = (x: DocumentationObject, y: DocumentationObject) => if (x.name.equalsIgnoreCase(y.name)) x.name2.toLowerCase().compareTo(y.name2.toLowerCase()) else x.name.toLowerCase().compareTo(y.name.toLowerCase())
}
