package tuxcalculator.core.value

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object MatrixOps {
  case class MatrixEntry(value: MathValue, row: Int, col: Int)
  
  def transform(mat: MathMatrix, transform: MatrixEntry => MathValue): MathValue = {
    val builder = new MatrixBuilder(mat.width, mat.height)
    for ((row, col) <- mat.indices) {
      builder(row, col) = transform(MatrixEntry(mat.get(row, col), row, col))
    }
    builder.build
  }
  
  def join(a: MathMatrix, b: MathMatrix, join: (MathValue, MathValue) => MathValue): MathValue = {
    if (a.width != b.width || a.height != b.height) {
      MathError("Matrix dimension mismatch: " + dim(a) + " and " + dim(b) + ".")
    } else {
      val builder = new MatrixBuilder(a.width, a.height)
      for ((row, col) <- a.indices) {
        builder(row, col) = join(a.get(row, col), b.get(row, col))
      }
      builder.build
    }
  }
  
  private def doMul(a: MathMatrix, b: MathMatrix): Either[MathMatrix, String] = {
    if (a.width != b.height) {
      Right("Can't multiply matrices: " + dim(a) + " and " + dim(b) + ".")
    } else {
      val builder = new MatrixBuilder(b.width, a.height)
      for (row <- 0 until a.height; col <- 0 until b.width) {
        builder(row, col) = (a.getRow(row) zip b.getCol(col)).map(entry => NumberHelper.mul(entry._1, entry._2)).reduce(NumberHelper.add)
      }
      builder.buildMatrix.map(Left.apply).getOrElse(Right("Matrix was not filled."))
    }
  }
  
  def mul(a: MathMatrix, b: MathMatrix): MathValue = doMul(a, b) match {
    case Left(mat) => mat
    case Right(err) => MathError(err)
  }
  
  def identity(size: Int): MathValue = {
    if (size <= 0) return MathError("Can't make identity matrix with size " + size)
    val builder = new MatrixBuilder(size, size)
    for (row <- 0 until size; col <- 0 until size) {
      if (row == col) builder(row, col) = MathNumber.One
      else builder(row, col) = MathNumber.Zero
    }
    builder.build
  }
  
  def invert(mat: MathMatrix): MathValue = {
    if (mat.width != mat.height) {
      MathError("Can't invert " + dim(mat) + " matrix.")
    } else {
      NumberHelper.mul(NumberHelper.div(MathNumber.One, det(mat)), adj(mat))
    }
  }
  
  @tailrec
  def raise(mat: MathMatrix, exp: Int): MathValue = {
    if (mat.width != mat.height) {
      MathError("Can't compute powers of " + dim(mat) + " matrix.")
    } else if (exp == 0) {
      identity(mat.width)
    } else if (exp > 0) {
      (0 until (exp - 1)).foldLeft[Either[MathMatrix, String]](Left(mat))((m, _) => m match {
        case Left(current) => doMul(current, mat)
        case Right(err) => Right(err)
      }) match {
        case Left(result) => result
        case Right(err) => MathError(err)
      }
    } else invert(mat) match {
      case inverted: MathMatrix => raise(inverted, -exp)
      case res => MathError("Can't invert matrix: got " + ValueHelper.calc.format(res))
    }
  }
  
  def det(mat: MathMatrix): MathValue = {
    def signedDet(row: Int, subMatrix: MathMatrix): MathValue = {
      if (row % 2 == 0) NumberHelper.add(MathNumber.Zero, det(subMatrix))
      else NumberHelper.sub(MathNumber.Zero, det(subMatrix))
    }
    
    if (mat.width != mat.height) MathError("Can't compute determinant of " + dim(mat) + " matrix.") else mat.width match {
      case 1 => mat.get(0, 0)
      case 2 => NumberHelper.sub(NumberHelper.mul(mat.get(0, 0), mat.get(1, 1)), NumberHelper.mul(mat.get(1, 0), mat.get(0, 1)))
      case _ => mat.getCol(0).zipWithIndex.map(entry => NumberHelper.mul(entry._1, signedDet(entry._2, minorAt(mat, entry._2, 0)))).reduce(NumberHelper.add)
    }
  }
  
  def adj(mat: MathMatrix): MathValue = {
    if (mat.width != mat.height) MathError("Can't compute adjugate of " + dim(mat) + " matrix.") else mat.width match {
      case 1 => MathMatrix(Vector(Vector(MathNumber.One)))
      case 2 =>
        val builder = new MatrixBuilder(2, 2)
        builder(0, 0) = mat.get(1, 1)
        builder(1, 1) = mat.get(0, 0)
        builder(1, 0) = NumberHelper.sub(MathNumber.Zero, mat.get(1, 0))
        builder(0, 1) = NumberHelper.sub(MathNumber.Zero, mat.get(0, 1))
        builder.buildMatrix.get
      case _ => ValueHelper.get(transform(mat, entry => {
        val detTranspose = det(minorAt(mat, entry.col, entry.row))
        val cofactor = if ((entry.row + entry.col) % 2 == 0) detTranspose else NumberHelper.sub(MathNumber.Zero, detTranspose)
        cofactor
      }))
    }
  }
  
  private def minorAt(matrix: MathMatrix, skipRow: Int, skipCol: Int): MathMatrix = {
    assert(matrix.width > 1 && matrix.height > 1, "Matrix of minors needs at least 2x2")
    assert(matrix.width == matrix.height, "Matrix of minors needs a square matrix")
    val builder = new MatrixBuilder(matrix.width - 1, matrix.height - 1)
    for (row <- 0 until matrix.height; col <- 0 until matrix.width if row != skipRow && col != skipCol) {
      val newRow = if (row < skipRow) row else row - 1
      val newCol = if (col < skipCol) col else col - 1
      builder(newRow, newCol) = matrix.get(row, col)
    }
    builder.buildMatrix.get
  }
  
  private def dim(matrix: MathMatrix): String = "" + matrix.height + "x" + matrix.width
  
  private class MatrixBuilder(width: Int, height: Int) {
    private val values: ListBuffer[ListBuffer[MathValue]] = ListBuffer.fill(width)(ListBuffer.fill(height)(MathVoid))
    def update(row: Int, col: Int, value: MathValue): Unit = values(col)(row) = value
    def buildMatrix: Option[MathMatrix] = if (values.exists(_.contains(MathVoid))) None else Some(MathMatrix(values.toVector.map(_.toVector)))
    def build: MathValue = buildMatrix.getOrElse(MathVoid)
  }
}
