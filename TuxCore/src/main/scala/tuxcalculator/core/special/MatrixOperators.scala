package tuxcalculator.core.special

import tuxcalculator.core.Calculator
import tuxcalculator.core.data.CalculatorSpecial
import tuxcalculator.core.value._

object MatrixOperators {

  object Wd extends CalculatorSpecial.SimpleFunction("wd", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      ValueHelper.get(args.head) match {
        case m@MathMatrix(_) => MathNumber(m.width)
        case _ => MathError("Can't get width of: " + calc.format(args.head))
      }
    }
  }

  object Ht extends CalculatorSpecial.SimpleFunction("ht", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      ValueHelper.get(args.head) match {
        case m@MathMatrix(_) => MathNumber(m.height)
        case _ => MathError("Can't get height of: " + calc.format(args.head))
      }
    }
  }

  object Mflat extends CalculatorSpecial.SimpleFunction("mflat", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      def stackOnTop(matrices: List[MathMatrix]): Option[MathMatrix] = matrices match {
        case Nil => None
        case mat :: Nil => Some(mat)
        case mat1 :: tail => stackOnTop(tail).flatMap(mat2 => (mat1, mat2) match {
          case (mat1, mat2) if mat1.width == mat2.width => Some(MathMatrix((0 until mat1.width).map(col => mat1.getCol(col) ++ mat2.getCol(col)).toVector))
          case _ => None
        })
      }
      
      def stackSideToSide(matrices: List[MathMatrix]): Option[MathMatrix] = matrices match {
        case Nil => None
        case mat :: Nil => Some(mat)
        case mat1 :: tail => stackOnTop(tail).flatMap(mat2 => (mat1, mat2) match {
          case (mat1, mat2) if mat1.height == mat2.height => Some(MathMatrix(mat1.values ++ mat2.values))
          case _ => None
        })
      }

      def all[T](opts: List[Option[T]]): Option[List[T]] = opts match {
        case Nil => Some(Nil)
        case None :: tail => None
        case Some(head) :: tail => all(tail).map(lst => head::lst)
      }
      
      val matrix = ValueHelper.matrix(args.head)
      val subMatrices: List[List[MathMatrix]] = matrix.values.map(_.map(ValueHelper.matrix).toList).toList
      
      all(subMatrices.transpose.map(stackSideToSide)).flatMap(stackOnTop) match {
        case Some(result) => result
        case None => all(subMatrices.map(stackOnTop)).flatMap(stackSideToSide) match {
          case Some(result) => result
          case None => MathError("Matrix dimension mismatch.")
        }
      }
    }
  }
  
  object Adj extends CalculatorSpecial.SimpleFunction("adj", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      val mat = ValueHelper.matrix(args.head)
      MatrixOps.adj(mat)
    }
  }
  
  object Det extends CalculatorSpecial.SimpleFunction("det", 1) {
    override protected def result(calc: Calculator, args: Vector[MathValue]): MathValue = ValueHelper.run(calc) {
      val mat = ValueHelper.matrix(args.head)
      MatrixOps.det(mat)
    }
  }
}
