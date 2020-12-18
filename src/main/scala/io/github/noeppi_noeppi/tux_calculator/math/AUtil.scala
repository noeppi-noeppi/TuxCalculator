package io.github.noeppi_noeppi.tux_calculator.math

import io.github.noeppi_noeppi.tux_calculator.math.parser.{FuncData, Parser}

import scala.collection.mutable.ListBuffer

object AUtil {

  def getBool(double: Double): Boolean = {
    if (double > 0) {
      true
    } else {
      false
    }
  }

  def fromBool(boolean: Boolean): Double = {
    if (boolean) {
      1
    } else {
      0
    }
  }

  def correctParams(func: MFunction, args: Int): Boolean = {
    if (func == null) {
      false
    } else if (args == Parser.VARARG && func.params == Parser.VARARG) {
      true
    } else if (args != func.params && func.params != Parser.VARARG) {
      false
    } else if (args == Parser.VARARG && args <= 0) {
      false
    } else {
      true
    }
  }

  def getBestParamCountMatching(params1: Int, params2: Int): Option[Int] = {
    if (params1 == 0 && params2 == 0) {
      Some(0)
    } else if (params1 == 0 || params2 == 0) {
      None
    } else if (params1 == params2) {
      Some(params1)
    } else if (params1 == Parser.VARARG) {
      Some(params2)
    } else if (params2 == Parser.VARARG) {
      Some(params1)
    } else {
      None
    }
  }

  def merge(params: Option[Int], params2: Int): Option[Int] = {
    params match {
      case None => Some(params2)
      case Some(x) => getBestParamCountMatching(x, params2)
    }
  }

  def getListLength(fp: FuncData, func: MFunction): Int = {
    val lengthDouble = func.result(fp, -1)
    if (lengthDouble.isNaN || lengthDouble.isInfinite || lengthDouble < 0) {
      -1
    } else {
      lengthDouble.toInt
    }
  }

  def getList(fp: FuncData, func: MFunction): Seq[Double] = {
    func match {
      case null => null
      case x: MFunctionList => x.values
      case x: MFunctionLazyList => x.values
      case x =>
        val lengthDouble = x.result(fp, -1)
        if (lengthDouble.isNaN || lengthDouble < 0) {
          null
        } else {
          val length = lengthDouble.toInt
          val list = ListBuffer[Double]()
          for (i <- 0 until length) {
            list += x.result(fp, i)
          }
          list.toList
        }
    }
  }
}
