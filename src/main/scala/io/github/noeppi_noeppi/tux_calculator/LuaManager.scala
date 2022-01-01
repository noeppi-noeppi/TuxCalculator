package io.github.noeppi_noeppi.tux_calculator

import java.io.StringReader
import io.github.noeppi_noeppi.tux_calculator.math.{AUtil, MFunction, MFunctionList}
import io.github.noeppi_noeppi.tux_calculator.math.parser.{FuncData, Parser}
import org.luaj.vm2.lib.VarArgFunction
import org.luaj.vm2.lib.jse.JsePlatform
import org.luaj.vm2.{LuaError, LuaFunction, LuaValue, Varargs}

import scala.collection.mutable.ListBuffer

object LuaManager {

  def parse(file: List[String], fadd: MFunction => Unit): Unit = {
    try {
      val modifiedFile = if (file.head.trim.toUpperCase == "__LUA__") {
        file.drop(1)
      } else {
        file
      }
      val globals = JsePlatform.standardGlobals

      globals.set("VARARG", Parser.VARARG)
      globals.set("getFunc", new VarArgFunction {
        override def invoke(args: Varargs): Varargs = getFunc(args)
      })
      globals.set("addFunc", new VarArgFunction {
        override def invoke(args: Varargs): Varargs = addFunc(args)
      })
      globals.set("getList", new VarArgFunction {
        override def invoke(args: Varargs): Varargs = getList(args)
      })
      globals.set("addList", new VarArgFunction {
        override def invoke(args: Varargs): Varargs = addList(args)
      })
      globals.set("toBool", new VarArgFunction {
        override def invoke(args: Varargs): Varargs = toBool(args)
      })
      globals.set("fromBool", new VarArgFunction {
        override def invoke(args: Varargs): Varargs = fromBool(args)
      })
      globals.set("adjustBool", new VarArgFunction {
        override def invoke(args: Varargs): Varargs = adjustBool(args)
      })

      val code = globals.load(new StringReader(modifiedFile.mkString(System.lineSeparator())), "@.tuxtr.rc")
      code.call()
      val export = globals.get("EXPORT").checktable()
      var i = 1
      while (!export.get(i).isnil()) {
        fadd(funcFromLuaDesc(export.get(i)))
        i += 1
      }
    } catch {
      case e: LuaError => e.printStackTrace()
    }
  }

  def getFunc(fargs: Varargs): Varargs = {
    val funcData = fargs.checkuserdata(1, classOf[FuncData]).asInstanceOf[FuncData]
    if (funcData == null) {
      throw new LuaError("No valid function-pointers")
    }
    val p = fargs.checkint(2)
    val mf = funcData.get(p)
    if (mf == null) {
      LuaValue.varargsOf(Array(LuaValue.NIL, LuaValue.NIL))
    } else {
      LuaValue.varargsOf(Array(new VarArgFunction {
        override def invoke(args: Varargs): Varargs = {
          val jargs = List.newBuilder[Double]
          var i = 1
          while (i <= args.narg()) {
            jargs += args.checkdouble(i)
            i += 1
          }
          val j = jargs.result()
          if ((mf.params != Parser.VARARG && mf.params != j.size) || (mf.params == Parser.VARARG && j.isEmpty)) {
            LuaValue.valueOf(Double.NaN)
          } else {
            LuaValue.valueOf(mf.result(funcData, j: _*))
          }
        }
      }, LuaValue.valueOf(mf.params)))
    }
  }

  def addFunc(fargs: Varargs): LuaValue = {
    val funcData = fargs.checkuserdata(1, classOf[FuncData]).asInstanceOf[FuncData]
    if (funcData == null) {
      throw new LuaError("No valid function-pointers")
    }
    val fimpl = fargs.checkfunction(2)
    val fparams = fargs.checkint(3)
    val fprovidefp = if (fargs.narg() >= 4) {
      fargs.checkboolean(4)
    } else {
      true
    }
    LuaValue.valueOf(funcData.add(new MFunctionLua("", fparams, fimpl, "", fprovidefp)))
  }

  def getList(fargs: Varargs): LuaValue = {
    val funcData = fargs.checkuserdata(1, classOf[FuncData]).asInstanceOf[FuncData]
    if (funcData == null) {
      throw new LuaError("No valid function-pointers")
    }
    val p = fargs.checkint(2)
    val mf = funcData.get(p)
    if (mf == null) {
      LuaValue.NIL
    } else {
      val list = ListBuffer[Double]()
      val lengthDouble = mf.result(funcData, -1)
      if (lengthDouble.isNaN || lengthDouble < 0) {
        return LuaValue.NIL
      }
      val length = lengthDouble.toInt
      for (i <- 0 until length) {
        list += mf.result(funcData, i)
      }
      val luaList = LuaValue.listOf(list.map(LuaValue.valueOf).toArray)
      luaList.set("n", length)
      luaList
    }
  }

  def addList(fargs: Varargs): LuaValue = {
    val funcData = fargs.checkuserdata(1, classOf[FuncData]).asInstanceOf[FuncData]
    if (funcData == null) {
      throw new LuaError("No valid function-pointers")
    }
    val luaList = fargs.checktable(2)
    val length = if (luaList.keys().exists(lv => lv.isstring() && lv.checkstring().tojstring() == "n")) {
      luaList.get("n").checkdouble().toInt
    } else {
      var i = 1
      while (!luaList.get(i).isnil()) {
        i += 1
      }
      i
    }
    val list = ListBuffer[Double]()
    for (i <- 1 to length) {
      list += luaList.get(i).checkdouble()
    }
    LuaValue.valueOf(funcData.add(new MFunctionList(list.toList)))
  }

  def toBool(fargs: Varargs): LuaValue = LuaValue.valueOf(AUtil.getBool(fargs.checkdouble(1)))
  def fromBool(fargs: Varargs): LuaValue = LuaValue.valueOf(AUtil.fromBool(fargs.checkboolean(1)))
  def adjustBool(fargs: Varargs): LuaValue = LuaValue.valueOf(AUtil.fromBool(AUtil.getBool(fargs.checkdouble(1))))

  def funcFromLuaDesc(luaValue: LuaValue): MFunction = {
    val struct = luaValue.checktable()

    val fkey = struct.get(1).checkstring().tojstring()
    val fimpl = struct.get(2).checkfunction()
    val fargs = struct.get(3).checknumber().toint()
    val fdoc = if (struct.length() >= 4) { struct.get(4).checkstring().tojstring() } else { "" }

    new MFunctionLua(fkey, fargs, fimpl, fdoc)
  }

  class MFunctionLua(val id: String, val args: Int, val func: LuaFunction, override val doc: String, val provideFP: Boolean = true) extends MFunction {

    override val name: String = id
    override val params: Int = args
    override def result(functionPointers: FuncData, param: Double*): Double = {
      try {
        if ((args != Parser.VARARG && args != param.size) || (args == Parser.VARARG && param.isEmpty)) {
          Double.NaN
        } else {
          val varargs = if (provideFP) {
            param.map(d => LuaValue.valueOf(d)).prepended(LuaValue.userdataOf(functionPointers)).toArray[LuaValue]
          } else {
            param.map(d => LuaValue.valueOf(d)).toArray[LuaValue]
          }
          val ret = func.invoke(varargs)
          if (ret.narg()<= 0) {
            Double.NaN
          } else {
            ret.checkdouble(1)
          }
        }
      } catch {
        case e: LuaError =>
          println("A function implemented in LUA raised an error: " + e.getMessage)
          println("The message is of type " + e.getMessageObject.`type`())
          Double.NaN
      }
    }
  }
}
