package io.github.noeppi_noeppi.tux_calculator

import scala.util.matching.Regex

object NumberStringer {

  val PRECENDING_ZERO: Regex = "^0+(.+?)$".r
  val TEN: Regex = "^([1-9])([0-9])$".r
  val HUNDRED: Regex = "^([1-9])([0-9]{2})$".r

  val REMOVE_SPACES: Regex = "(.*?)\\s*".r

  val TRAILING_ZERO: Regex = "^(.+?)0+$".r

  def toString(number: String): String = {
    var str = if (number.contains(".")) {
      intToString(number.substring(0, number.indexOf('.'))) + toFloatString(number.substring(number.indexOf('.') + 1, number.length))
    } else {
      intToString(number)
    }
    while (str.startsWith("minus minus ")) {
      str = str.substring(12)
    }
    if (str == "minus null")
      "null"
    else
      str
  }

  @scala.annotation.tailrec
  def toFloatString(number: String): String = number match {
    case TRAILING_ZERO(f) => toFloatString(f)
    case "0" => ""
    case f =>
      val sb = new StringBuilder
      sb append " komma"
      for (c <- f)
        sb append " " append numberChar(c)
      sb.toString
  }

  def intToString(int: String): String = {
    int match {
      case x if x.startsWith("-") => return "minus " + toString(x.substring(1, x.length))
      case "0" => return "null"
      case PRECENDING_ZERO(x) => return toString(x)
      case _ =>
    }
    var group = 0
    val sb = new StringBuilder
    while (group * 3 < int.length) {
      val block = int.substring(Math.max(0, int.length - (group * 3) - 3), int.length - (group * 3))
      sb.insert(0, get(group, block))
      group += 1
    }
    val str = sb.toString
    str match {
      case REMOVE_SPACES(s) => s
      case s => s
    }
  }

  def numberChar(number: Char): String = number match {
    case '0' => "null"
    case '1' => "eins"
    case '2' => "zwei"
    case '3' => "drei"
    case '4' => "vier"
    case '5' => "fünf"
    case '6' => "sechs"
    case '7' => "sieben"
    case '8' => "acht"
    case '9' => "neun"
  }

  def group(int: String, prefix: String = "", suffix: String = "", suffix1: String = "s", suffix0: String = ""): String = {
    int match {
      case "0" => return "";
      case PRECENDING_ZERO(i) => return group(i, prefix, suffix, suffix1, suffix0)
      case "1" => return prefix + "ein" + suffix1 + suffix;
      case _ =>
    }
    val str = int match {
      case "2" => "zwei"
      case "3" => "drei"
      case "4" => "vier"
      case "5" => "fünf"
      case "6" => "sechs"
      case "7" => "sieben"
      case "8" => "acht"
      case "9" => "neun"
      case "10" => "zehn"
      case "11" => "elf"
      case "12" => "zwölf"
      case TEN("1", i) => group(i, suffix1 = "") + "zehn"
      case TEN("2", i) => group(i, suffix = "und", suffix1 = "") + "zwanzig"
      case TEN("3", i) => group(i, suffix = "und", suffix1 = "") + "dreißig"
      case TEN("4", i) => group(i, suffix = "und", suffix1 = "") + "vierzig"
      case TEN("5", i) => group(i, suffix = "und", suffix1 = "") + "fünfzig"
      case TEN("6", i) => group(i, suffix = "und", suffix1 = "") + "sechzig"
      case TEN("7", i) => group(i, suffix = "und", suffix1 = "") + "siebzig"
      case TEN("8", i) => group(i, suffix = "und", suffix1 = "") + "achtzig"
      case TEN("9", i) => group(i, suffix = "und", suffix1 = "") + "neunzig"
      case HUNDRED(x, i) => group(x, suffix1 = "") + "hundert" + group(i)
    }
    prefix + str + suffix + suffix0
  }

  def get(block: Int, int: String): String = block match {
    case _ if int == "0" || int == "00" || int == "000" => ""
    case 0 => group(int)
    case 1 => group(int, suffix = "tausend")
    case i if i % 2 == 0 => group(int, suffix = getSuffixRaw(i / 2) + "llion", suffix0 = "en", suffix1 = "e ") + " "
    case i => group(int, suffix = getSuffixRaw((i - 1) / 2) + "lliarde", suffix0 = "n", suffix1 = "e ") + " "
  }

  def getSuffixRaw(block: Int): String = block match {
    case 1 => "mi"
    case 2 => "bi"
    case 3 => "tri"
    case 4 => "quadri"
    case 5 => "quinti"
    case 6 => "sexti"
    case 7 => "septi"
    case 8 => "okti"
    case 9 => "noni"
    case i if i >= 15 && (i - 15) % 100 == 0 => getSuffixLli(i).replace("quinquadezi", "quindezi") // Ausnahme
    case i if i >= 1000 => getSuffixLli(i)
    case i => getSuffixCombinator(i, "")
  }

  def getSuffixLli(block: Int): String = block match {
    case 0 => "ni"
    case i if i >= 1000 => getSuffixRaw(i / 1000) + "lli" + getSuffixLli(i % 1000)
    case i => getSuffixCombinator(i, "")
  }

  def getSuffixCombinator(block: Int, accepted_suffix: String): String = block match {
    case 0 => ""
    case 1 => "un"
    case 2 => "duo"
    case 3 if accepted_suffix.contains('s') => "tres"
    case 3 => "tre"
    case 4 => "quattuor"
    case 5 => "quinqua"
    case 6 if accepted_suffix.contains('x') => "sex" //WICHTIG ERST X, SONST IST 106 = seszenti statt sexzenti
    case 6 if accepted_suffix.contains('s') => "ses"
    case 6 => "se"
    case 7 if accepted_suffix.contains('m') => "septem"
    case 7 if accepted_suffix.contains('n') => "septen"
    case 7 => "septe"
    case 8 => "okto"
    case 9 if accepted_suffix.contains('m') => "novem"
    case 9 if accepted_suffix.contains('n') => "noven"
    case 9 => "nove"
    case i if 10 <= i && i < 20 => getSuffixCombinator(block - 10, "n") + "dezi"
    case i if 20 <= i && i < 30 => getSuffixCombinator(block - 20, "ms") + "viginti"
    case i if 30 <= i && i < 40 => getSuffixCombinator(block - 30, "ns") + "triginta"
    case i if 40 <= i && i < 50 => getSuffixCombinator(block - 40, "ns") + "quadraginta"
    case i if 50 <= i && i < 60 => getSuffixCombinator(block - 50, "ns") + "quinquaginta"
    case i if 60 <= i && i < 70 => getSuffixCombinator(block - 60, "n") + "sexaginta"
    case i if 70 <= i && i < 80 => getSuffixCombinator(block - 70, "n") + "septuaginta"
    case i if 80 <= i && i < 90 => getSuffixCombinator(block - 80, "mx") + "oktoginta"
    case i if 90 <= i && i < 100 => getSuffixCombinator(block - 90, "") + "nonaginta"
    case i if 100 <= i && i < 200 => getSuffixCombinator(block - 100, "nxs") + "zenti"
    case i if 200 <= i && i < 300 => getSuffixCombinator(block - 200, "n") + "duzenti"
    case i if 300 <= i && i < 400 => getSuffixCombinator(block - 300, "ns") + "trezenti"
    case i if 400 <= i && i < 500 => getSuffixCombinator(block - 400, "ns") + "quadringenti"
    case i if 500 <= i && i < 600 => getSuffixCombinator(block - 500, "ns") + "quingenti"
    case i if 600 <= i && i < 700 => getSuffixCombinator(block - 600, "n") + "seszenti"
    case i if 700 <= i && i < 800 => getSuffixCombinator(block - 700, "n") + "septingenti"
    case i if 800 <= i && i < 900 => getSuffixCombinator(block - 800, "mx") + "oktingenti"
    case i if 900 <= i && i < 1000 => getSuffixCombinator(block - 900, "") + "nongenti"
  }
}
