package io.github.noeppi_noeppi.tux_calculator.math.parser

import io.github.noeppi_noeppi.tux_calculator.math.MFunction

import scala.collection.mutable

class FuncData() {

  private val funcMap = mutable.Map[Int, MFunction](
    0 -> FuncData.IDENTITY_FUNC,
    1 -> FuncData.EMPTY_LIST
  )
  private var nextFid = funcMap.size

  def get(pointer: Int): MFunction = {
    funcMap.getOrElse(pointer, null)
  }

  def get(func: MFunction): Int = {
    funcMap.foldLeft(-1)((fid, entry) => if (fid < 0 && entry._2 == func) entry._1 else fid)
  }

  def add(func: MFunction): Int = {
    if (funcMap.forall(entry => func != entry._2)) {
      val fid = nextFid
      nextFid += 1
      funcMap.put(fid, func)
      fid
    } else {
      get(func)
    }
  }
}

object FuncData {
  val IDENTITY_FUNC: MFunction = new MFunction {
    override val name: String = ""
    override val params: Int = 1

    override def result(functionPointers: FuncData, param: Double*): Double = param(0)

    override val name2: String = "Identity"
    override val doc: String = "Erwartet ein Argument und gibt dieses zurück."
  }

  val EMPTY_LIST: MFunction = new MFunction {
    override val name: String = ""
    override val params: Int = 1

    override def result(functionPointers: FuncData, param: Double*): Double = {
      if (param(0) == -1) {
        0
      } else {
        Double.NaN
      }
    }

    override val name2: String = "Leere Liste"
    override val doc: String = "Eine leere Liste"
  }
}