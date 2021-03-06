package io.github.noeppi_noeppi.tux_calculator.math.parser

import java.nio.file.Paths
import io.github.noeppi_noeppi.tux_calculator.LuaManager
import io.github.noeppi_noeppi.tux_calculator.math.{Constant, DocumentationObject, MDerivedFunction, MFunction, Operator, PostfixUnary, Priority, Unary}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._
import scala.util.matching.Regex

class Parser private(val const: Map[String, Constant], val func: Map[Int, Map[String, MFunction]], val op: Map[String, Operator], val unary: Map[String, Unary], val postfixUnary: Map[String, PostfixUnary], val docElems: Set[DocumentationObject], private val additionalDocElems: Set[DocumentationObject], val funcData: FuncData) {

  private val vars = mutable.Map[String, Double]()
  private val checkOp = new CheckOperator(this)
  private val unaries = "[" + (unary.foldLeft("")((s, kv) => if (kv._1.length == 1 && !kv._1.charAt(0).isLetterOrDigit) s + kv._1 else s).replace("^", "\\^").replace("-", "\\-").replace("[", "\\[").replace("]", "\\]")) + "]"
  private val postfixUnaries = "[" + (postfixUnary.foldLeft("")((s, kv) => if (kv._1.length == 1 && !kv._1.charAt(0).isLetterOrDigit) s + kv._1 else s).replace("^", "\\^").replace("-", "\\-").replace("[", "\\[").replace("]", "\\]")) + "]"
  private val charOperators = "[" + (op.foldLeft("")((s, kv) => if (kv._1.forall(c => !c.isLetterOrDigit)) s + kv._1 else s).replace("^", "\\^").replace("-", "\\-").replace("[", "\\[").replace("]", "\\]")) + "]"
  private val charOperatorStr = op.foldLeft("")((s, kv) => if (kv._1.forall(c => !c.isLetterOrDigit)) s + kv._1 else s) + "`"

  private val R_UNARY = if (unary.isEmpty) { "$a".r } else { M.UNARY.replace("%%", unaries).r }
  private val R_POSTFIX_UNARY = if (postfixUnary.isEmpty) { "$a".r } else { M.POSTFIX_UNARY.replace("%%", postfixUnaries).r }
  private val R_OPERATOR = if (op.isEmpty) { "$a".r } else { M.OPERATOR.replace("%%", charOperators).r }

  private val customFunc = mutable.Map[Int, mutable.Map[String, MFunction]]()
  private val customOp = mutable.Map[String, Operator]()
  private val customUnary = mutable.Map[String, Unary]()
  private val customPostfixUnary = mutable.Map[String, PostfixUnary]()

  private val undefinedSym = mutable.Set[String]()

  private var last = 0d

  def this(a: List[Constant], b: List[MFunction], d: List[Operator], e: List[Unary], f: List[PostfixUnary], g: collection.Map[String, Double], h: Set[DocumentationObject], fd: FuncData) = {
    this(
      Map(a.map(a => a.name -> a): _*),
      b.groupBy(a => a.params).map(a => a._1 -> Map.from(a._2.map(a => a.name -> a))),
      Map(d.map(a => a.name -> a): _*),
      Map(e.map(a => a.name -> a): _*),
      Map(f.map(a => a.name -> a): _*),
      Set.newBuilder[DocumentationObject].addAll(a).addAll(b).addAll(d).addAll(e).addAll(f).addAll(h).result(),
      h, fd)
    vars.addAll(g)
    b.foreach(mf => funcData.add(mf))
  }

  def derive(initFile: List[String]): Parser = {
    if (initFile.exists(s => s.trim.toUpperCase() == "__LUA__")) {
      val iterables = initFile.splitAt(initFile.indexWhere(s => s.trim.toUpperCase() == "__LUA__"))
      LuaManager.parse(iterables._2, mf => {
        customFunc.getOrElseUpdate(mf.params, mutable.Map()) += mf.name -> mf
        funcData.add(mf)
      })
      createExtendedClosure().derive(iterables._1)
    } else {
      var line = 0
      //noinspection DangerousCatchAll
      try {
        for (command <- initFile) {
          line += 1
          if (command.trim.nonEmpty && !command.trim.startsWith("#"))
            rcParse(command.trim)
        }
        createExtendedClosure()
      } catch {
        case x: Exception =>
          x.setStackTrace(Array(new StackTraceElement(Paths.get(System.getProperty("user.home")).resolve(".tuxtr").toAbsolutePath.normalize().toString, "rc", ".tuxtr.rc", line)) ++ x.getStackTrace)
          throw x
      }
    }
  }

  private def rcParse(eq: String): Double = {
    try {
      if (eq.contains("{") || eq.contains("}"))
        throw new IllegalArgumentException("eq contained one of { and }")
      last = eq match {
        case M.OPERATOR_DECLARATION_A(ra, name, pp1, pp2, term) => addOp(name, Priority.ADDITIVE, ra.toBoolean, pp1, pp2, term); 0
        case M.OPERATOR_DECLARATION_M(ra, name, pp1, pp2, term) => addOp(name, Priority.MULTIPLICATIVE, ra.toBoolean, pp1, pp2, term); 0
        case M.OPERATOR_DECLARATION_P(ra, name, pp1, pp2, term) => addOp(name, Priority.POWER, ra.toBoolean, pp1, pp2, term); 0
        case M.UNARY_DECLARATION(name, pp1, term) => addUnary(name, pp1, term); 0
        case M.POSTUNARY_DECLARATION(name, pp1, term) => addPostfixUnary(name, pp1, term); 0
        case _ => parse(eq)
      }
      last
    } catch {
      case x: IllegalArgumentException => last = Double.NaN; throw x
    }
  }

  def parse(eq: String): Double = {
    try {
      if (eq.contains("{") || eq.contains("}"))
        throw new IllegalArgumentException("eq contained one of { and }")
      if (M.FUNCTION_DECLARATION.matches(eq)) {
        val mat = M.FUNCTION_DECLARATION.findFirstMatchIn(eq).get
        last = addFunc(mat.group(1), mat.group(2), mat.group(mat.groupCount))
      } else if (M.NOARG_FUNCTION_DECLARATION.matches(eq)) {
        val mat = M.NOARG_FUNCTION_DECLARATION.findFirstMatchIn(eq).get
        last = addFunc(mat.group(1), "", mat.group(2))
      } else if (M.FUNCTION_POINTER_DECLARATION.matches(eq)) {
        val mat = M.FUNCTION_POINTER_DECLARATION.findFirstMatchIn(eq).get
        val fp = doParse(mat.group(2)).toInt
        val baseFunc = funcData.get(fp)
        val fname = mat.group(1)
        if (baseFunc == null || fname.isEmpty) {
          last = -1
        } else {
          val func = new MDerivedFunction(fname, baseFunc)
          customFunc.getOrElseUpdate(baseFunc.params, mutable.Map()) += func.name -> func
          last = fp
        }
      } else {
        last = eq match {
          case x if x.contains("|") => x.split("\\|").filter(str => str.nonEmpty).foreach(str => parse(str)); last
          case M.VARIABLE_DECLARATION(key, value) => val x = doParse(value); vars += key -> x; x
          case M.RESET() => vars.clear(); customFunc.clear(); undefinedSym.clear(); 0
          case M.VARIABLE_CLEAR(key) => vars.remove(key); customFunc.foreach(m => m._2.remove(key)); undefinedSym.remove(key); 0
          case M.UNDEFINE(key) => undefinedSym += key; 0
          case _ => doParse(eq)
        }
      }
      last
    } catch {
      case x: IllegalArgumentException => last = Double.NaN; throw x
    }
  }

  private def getNextParen(eq: String): Int = {
    var openBrackets = 0
    for (pos <- eq.indices) eq.charAt(pos) match {
      case '[' => openBrackets += 1
      case ']' => if (openBrackets > 0) openBrackets -= 1 else throw new IllegalArgumentException("Too many closing brackets")
      case '(' => if (openBrackets == 0) return pos
      case _ =>
    }
    if (openBrackets > 0)
      throw new IllegalArgumentException("Not enought closing brackets")
    -1
  }

  private def closingParenLeft(eq: String): Boolean = {
    var openBrackets = 0
    for (pos <- eq.indices) eq.charAt(pos) match {
      case '[' => openBrackets += 1
      case ']' => if (openBrackets > 0) openBrackets -= 1 else throw new IllegalArgumentException("Too many closing brackets")
      case ')' => if (openBrackets == 0) return true
      case _ =>
    }
    if (openBrackets > 0)
      throw new IllegalArgumentException("Not enought closing brackets")
    false
  }

  private def doParse(eq: String): Double = {
    var eeq = eq

    breakable { while (true) {
      val begin = getNextParen(eeq)
      if (begin < 0)
        break()
      var end = -1
      var open = 1
      breakable {
        for (i <- 1 until eeq.length - begin) {
          eeq.charAt(begin + i) match {
            case '(' => open += 1;
            case ')' => open -= 1;
            case _ =>
          }; if (open <= 0) {
            end = begin + i; break();
          }
        }
      }
      if (end < 0) throw new IllegalArgumentException("Not enought closing parens")
      eeq = eeq.substring(0, begin) + doParseFuncParams(eeq.substring(begin + 1, end)) + eeq.substring(end + 1, eeq.length)
    } }
    if (closingParenLeft(eeq))
      throw new IllegalArgumentException("Too many closing parens")

    eeq match {
      case M.SPACES1(q) => doParse(q)
      case M.SPACES2(q) => doParse(q)
      case M.NUMBER(num, _*) => java.lang.Double.parseDouble(num)
      case M.PRECALC_NUMBER(num, _*) => java.lang.Double.parseDouble(num)
      case M.ANON_POINTER_IDENTITY() => funcData.get(FuncData.IDENTITY_FUNC)
      case M.ANON_POINTER(sig, impl) => addFunc("", sig, impl)
      case M.ANON_SHORT_POINTER(impl) => addFunc("", "x", impl)
      case M.FUNCTION(name, q) if hasFunc(name, q) => applyFunc(name, q)
      case M.POINTER(not, _) => getFuncPointer(not)
      case R_UNARY(name, q) if unary.contains(name) && !undefinedSym.contains(name) => unary(name).apply(doParse(q))
      case checkOp(q1, name, q2) => op(name).apply(doParse(q1), doParse(q2))
      case R_POSTFIX_UNARY(q, name) if postfixUnary.contains(name) && !undefinedSym.contains(name) => postfixUnary(name).apply(doParse(q))
      case M.LAST() => last
      case M.VARIABLE(name) if vars.contains(name) && !undefinedSym.contains(name) => vars(name)
      case M.CONST(name) if const.contains(name) && !undefinedSym.contains(name) => const(name).value
      case x => throw new IllegalArgumentException("Did not match anything: " + x)
    }
  }

  private def doParseFuncParams(eq: String): String = {
    var parens = 0
    var brackets = 0
    var lastIdx = 0
    val lb = ListBuffer[String]()
    for (idx <- eq.indices) {
      eq.charAt(idx) match {
        case '(' => parens += 1
        case ')' => parens = Math.max(0, parens - 1)
        case '[' => brackets += 1
        case ']' => brackets = Math.max(0, brackets - 1)
        case ',' if parens == 0 && brackets == 0 => lb += eq.substring(lastIdx, idx); lastIdx = idx + 1
        case _ =>
      }
    }
    lb += eq.substring(lastIdx, eq.length)
    lb.filter(str => str.nonEmpty).map(str => doParse(str).toString).mkString("{", ",", "}")
  }

  private def hasFunc(name: String, precalc: String): Boolean = {
    val params = if (precalc.trim.isEmpty) { 0 } else { precalc.count(c => c == ',') + 1 }
    if (params == 0) {
      (func.getOrElse(params, Map()).contains(name) || customFunc.getOrElse(params, mutable.Map()).contains(name)) && !undefinedSym.contains(name)
    } else {
      (func.getOrElse(params, Map()).contains(name) || customFunc.getOrElse(params, mutable.Map()).contains(name)
        || func.getOrElse(Parser.VARARG, Map()).contains(name) || customFunc.getOrElse(Parser.VARARG, mutable.Map()).contains(name)) && !undefinedSym.contains(name)
    }
  }
  private def applyFunc(name: String, precalc: String): Double = {
    val params = if (precalc.trim.isEmpty) { 0 } else { precalc.count(c => c == ',') + 1 }
    var f = customFunc.getOrElse(params, mutable.Map()).getOrElse(name, func.getOrElse(params, Map()).getOrElse(name, null))
    if (f == null && params != 0)
      f = customFunc.getOrElse(Parser.VARARG, mutable.Map()).getOrElse(name, func.getOrElse(Parser.VARARG, Map()).getOrElse(name, null))
    if (f == null)
      0
    else
      f.result(funcData, ArraySeq.from(precalc.split(",").filter(s => s.nonEmpty).map(s => s.trim.toDouble)): _*)
  }
  private def addFunc(fname: String, pp1: String, term: String): Int = {
    val closure = createClosure()
    val args = List.from(pp1.split(",").filter(s => s.nonEmpty).map(s => s.trim))
    val func = new MFunction {
      override val name: String = fname
      override val params: Int = args.size
      override def result(functionPointers: FuncData, param: Double*): Double = {
        closure.vars.clear()
        for (i <- args.indices) {
          closure.vars.put(args(i), param(i))
        }
        val r = closure.parse(term)
        closure.vars.clear()
        r
      }
    }
    if (fname.nonEmpty)
      customFunc.getOrElseUpdate(args.size, mutable.Map()) += fname -> func
    funcData.add(func)
  }

  private def getFuncPointer(not: String): Int = {
    if (not.contains("#")) {
      val params = not.substring(not.indexOf("#") + 1).toInt
      val name = not.substring(0, not.indexOf("#"))
      val mf = customFunc.getOrElse(params, mutable.Map()).getOrElse(name, func.getOrElse(params, Map()).getOrElse(name, null))
      if (mf == null || undefinedSym.contains(mf.name)) {
        -1
      } else {
        funcData.get(mf)
      }
    } else {
      var minParam = Integer.MAX_VALUE
      var mf: MFunction = null
      for (entry <- customFunc) {
        if (entry._2.contains(not) && entry._1 < minParam) {
          minParam = entry._1
          mf = entry._2(not)
        }
      }
      for (entry <- func) {
        if (entry._2.contains(not) && entry._1 < minParam) {
          minParam = entry._1
          mf = entry._2(not)
        }
      }
      if (mf == null || undefinedSym.contains(mf.name)) {
        -1
      } else {
        funcData.get(mf)
      }
    }
  }

  private def addOp(fname: String, pr: Priority, ra: Boolean, pp1: String, pp2: String, term: String): Unit = {
    val closure = createClosure()
    val func = new Operator {
      override val name: String = fname
      override val priority: Priority = pr
      override val rightAssoc: Boolean = ra
      override def apply(op1: Double, op2: Double): Double = {
        closure.vars.clear()
        closure.vars.put(pp1, op1)
        closure.vars.put(pp2, op2)
        val r = closure.parse(term)
        closure.vars.clear()
        r
      }
    }
    customOp += fname -> func
  }

  private def addUnary(fname: String, pp1: String, term: String): Unit = {
    val closure = createClosure()
    val func = new Unary {
      override val name: String = fname
      override def apply(op: Double): Double = {
        closure.vars.clear()
        closure.vars.put(pp1, op)
        val r = closure.parse(term)
        closure.vars.clear()
        r
      }
    }
    customUnary += fname -> func
  }

  private def addPostfixUnary(fname: String, pp1: String, term: String): Unit = {
    val closure = createClosure()
    val func = new PostfixUnary {
      override val name: String = fname
      override def apply(op: Double): Double = {
        closure.vars.clear()
        closure.vars.put(pp1, op)
        val r = closure.parse(term)
        closure.vars.clear()
        r
      }
    }
    customPostfixUnary += fname -> func
  }

  private def createClosure(): Parser = {
    val pb = ParserBuilder().addAll(const.removedAll(undefinedSym).removedAll(vars.keys).values)
      .addAll(Map.from(vars).removedAll(undefinedSym).map(vd => new Constant {
        override val name: String = vd._1
        override val value: Double = vd._2
      }))
      .addAll(op.removedAll(undefinedSym).values)
      .addAll(unary.removedAll(undefinedSym).values)
      .addAll(postfixUnary.removedAll(undefinedSym).values)
      .addAll(additionalDocElems)
      for (fmap <- func) {
        pb.addAll(fmap._2.removedAll(undefinedSym).removedAll(customFunc.getOrElse(fmap._1, mutable.Map()).keys).values)
      }
      for (fmap <- customFunc) {
        pb.addAll(Map.from(fmap._2).removedAll(undefinedSym).values)
      }
    pb.extend(this)
  }

  private def createExtendedClosure(): Parser = { // This one adds the created operators and unaries.
    val pb = ParserBuilder().addAll(const.removedAll(undefinedSym).removedAll(vars.keys).values)
      .addAll(Map.from(vars).removedAll(undefinedSym).map(vd => new Constant {
        override val name: String = vd._1
        override val value: Double = vd._2
      }))
      .addAll(op.removedAll(undefinedSym).removedAll(customOp.keys).values)
      .addAll(unary.removedAll(undefinedSym).removedAll(customUnary.keys).values)
      .addAll(postfixUnary.removedAll(undefinedSym).removedAll(customPostfixUnary.keys).values)
      .addAll(Map.from(customOp).removedAll(undefinedSym).values)
      .addAll(Map.from(customUnary).removedAll(undefinedSym).values)
      .addAll(Map.from(customPostfixUnary).removedAll(undefinedSym).values)
      .addAll(additionalDocElems)
    for (fmap <- func) {
      pb.addAll(fmap._2.removedAll(undefinedSym).removedAll(customFunc.getOrElse(fmap._1, mutable.Map()).keys).values)
    }
    for (fmap <- customFunc) {
      pb.addAll(Map.from(fmap._2).removedAll(undefinedSym).values)
    }
    pb.extend(this)
  }

  private class CheckOperator(val parser: Parser) {
    def unapplySeq(arg: String): Option[List[String]] = {
      var last = 0
      var now = arg.indexWhere(x => parser.charOperatorStr.contains(x))
      var before: String = null
      var op: Operator = null
      var after: String = null
      while (now >= 0) {
        val b = arg.substring(0, now)
        arg.substring(now) match {
          case parser.R_OPERATOR(name) if parser.op.contains(name) && (!undefinedSym.contains(name)) && b.count(_ == '{') == b.count(_ == '}') =>
            if (op == null || parser.op(name).priority.shouldEvaluateFirst(op.priority, op.rightAssoc)) {
              op = parser.op(name); before = b; after = arg.substring(now + name.length)
          }
          case _ =>
        }
        val tmp = now
        now = arg.indexWhere(x => parser.charOperatorStr.contains(x), last + 1)
        last = tmp
      }
      if (op == null)
        None
      else
        Some(List(before, op.name, after))
    }
  }

  def allTabComplete: List[String] = Set.newBuilder
    .addAll(customFunc.values.flatten.map(entry => entry._1))
    .addAll(func.values.flatten.map(entry => entry._1))
    .addAll(const.values.map(c => c.name))
    .addAll(vars.keySet)
    .result().removedAll(undefinedSym).toList.sorted((x: String, y: String) => x.toLowerCase().compareTo(y.toLowerCase))

  def functionTabComplete: List[String] = Set.newBuilder
    .addAll(customFunc.values.flatten.map(entry => entry._1))
    .addAll(func.values.flatten.map(entry => entry._1))
    .result().removedAll(undefinedSym).toList.sorted((x: String, y: String) => x.toLowerCase().compareTo(y.toLowerCase))

  private object M {
    val BBW = "[A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]".r
    val BBWO = "[A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_+\\-*/%^~]".r
    val BBWN = "[A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_0-9]".r
    val BBWNO = "[A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_0-9+\\-*/%^~]".r
    val OP = "([+\\-*/%^~]|`[A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_+\\-*/%^~][A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_0-9+\\-*/%^~]*)`".r

    val VARIABLE_DECLARATION: Regex = "^\\s*let\\s+([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*=\\s*(.*)$".r
    val NOARG_FUNCTION_DECLARATION: Regex = "^\\s*def\\s+([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\(\\s*\\)\\s*=\\s*(.*)$".r
    val FUNCTION_DECLARATION: Regex = "^\\s*def\\s+([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\((\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*(,\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*)*)\\)\\s*=\\s*(.*)$".r
    val FUNCTION_POINTER_DECLARATION: Regex = "^\\s*defp\\s+([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*=\\s*(.*)$".r
    val OPERATOR_DECLARATION_A: Regex = "^\\s*def\\s+op\\s+additive\\s+(true|false)\\s+([`+\\-*/%~^!°A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\(\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*,\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\)\\s*=\\s*(.*)$".r
    val OPERATOR_DECLARATION_M: Regex = "^\\s*def\\s+op\\s+multiplicative\\s+(true|false)\\s+([`+\\-*/%~^!°A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\(\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*,\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\)\\s*=\\s*(.*)$".r
    val OPERATOR_DECLARATION_P: Regex = "^\\s*def\\s+op\\s+power\\s+(true|false)\\s+([`+\\-*/%~^!°A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\(\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*,\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\)\\s*=\\s*(.*)$".r
    val UNARY_DECLARATION: Regex = "^\\s*def\\s+unary\\s+([+\\-*/%~^!°A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\(\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\)\\s*=\\s*(.*)$".r
    val POSTUNARY_DECLARATION: Regex = "^\\s*def\\s+postfix\\s+([+\\-*/%~^!°A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\(\\s*([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_]+)\\s*\\)\\s*=\\s*(.*)$".r

    val VARIABLE_CLEAR: Regex = "^\\s*clear\\s+(.+?)\\s*$".r
    val UNDEFINE: Regex = "^\\s*undef\\s+(.+?)\\s*$".r
    val RESET: Regex = "^\\s*clearall\\s*$".r

    val SPACES1: Regex = "^\\s+(.*)$".r
    val SPACES2: Regex = "^(.*?)\\s+".r
    val NUMBER: Regex = "^(-?\\d+(\\.\\d+)?([Ee]-?\\d+)?)$".r
    val PRECALC_NUMBER: Regex = "^\\{((-?\\d+(\\.\\d+)?([Ee]-?\\d+)?)|(NaN|Infinity|-Infinity))\\}$".r
    val FUNCTION: Regex = "^([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_][A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_0-9+\\-*/%^~]*)\\s*\\{([^{]*?)\\}$".r
    val UNARY: String = "^(%%)\\s*(.+)$"
    val POSTFIX_UNARY: String = "^(.+?)\\s*(%%)$"
    val POINTER: Regex = "^@([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_][A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_0-9+\\-*/%^~]*(#\\d+)?)$".r
    val ANON_POINTER: Regex = "^@\\[\\s*(.*?)\\s*=\\s*(.*)\\s*\\]$".r
    val ANON_SHORT_POINTER: Regex = "^@\\[\\s*(.*)\\s*\\]$".r
    val ANON_POINTER_IDENTITY: Regex = "^@\\[\\s*\\]$".r
    val LAST: Regex = "^ans$".r
    val CONST: Regex = "^([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_][A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_0-9]*)$".r
    val VARIABLE: Regex = "^([A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_][A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_0-9]*)$".r

    val OPERATOR: String = "^(%%|`[A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_+\\-*/%^~][A-Za-zÀ-ž\\u0370-\\u03FF\\u0400-\\u04FF_0-9+\\-*/%^~]*?`).+?$"
  }
}

object Parser {

  val VARARG: Int = -1
}