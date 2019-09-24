package bitstream.types

import math._
import math.Numeric.Implicits._
import math.Numeric._
import Array._
import reflect._
import scala.reflect.runtime.universe._
import scala.util.Random
import bitstream.simulator.Operators._

trait MatrixSpecificType[A] {
  def div: Function2[A, A, A]
  def sqrt: Function1[A, A]
  def sqrtDouble: Function1[A, Double]
  def sqrtBS: Function1[A, Bitstream]
  def rand: Function0[A]
}

object MatrixSpecificType {
  implicit object MatrixInt extends MatrixSpecificType[Int] {
    def div = _/_
    def sqrt = math.sqrt(_).toInt
    def sqrtDouble = math.sqrt(_)
    def sqrtBS = (n: Int) => SBitstream(math.sqrt(n))
    def rand = () => math.random().toInt
  }

  implicit object MatrixLong extends MatrixSpecificType[Long] {
    def div = _/_
    def sqrt = math.sqrt(_).toLong
    def sqrtDouble = math.sqrt(_)
    def sqrtBS = (n: Long) => SBitstream(math.sqrt(n))
    def rand = () => math.random().toLong
  }

  implicit object MatrixDouble extends MatrixSpecificType[Double] {
    def div = _/_
    def sqrt = math.sqrt
    def sqrtDouble = math.sqrt
    def sqrtBS = (n: Double) => SBitstream(math.sqrt(n))
    def rand = math.random
  }

  implicit object MatrixBitstream extends MatrixSpecificType[SBitstream] {
    def div = _/_
    def sqrt = SBitstream.sqrt
    def sqrtDouble = SBitstream.sqrt(_).value
    def sqrtBS = SBitstream.sqrt
    def rand = () => SBitstream(math.random())
  }
}

case class Matrix[A: ClassTag] (val _numRows:Int, val _numCols:Int)(implicit num: Numeric[A]) {

  private var _array = Array.ofDim[A](_numRows, _numCols)

  def rows: Int = _numRows

  def cols: Int = _numCols

  def size: (Int, Int) = (_numRows, _numCols)

  def apply(row: Int, col: Int): A = _array(row)(col)
  def apply(index: Int): A = (_numRows, _numCols) match {
    case (1, _) => _array(0)(index)
    case (_, 1) => _array(index)(0)
    case _ => throw new IllegalArgumentException("Cannot access matrix with only one index")
  }
  def update(row: Int, col: Int, value: A) = {
    _array(row)(col) = value
  }
  def update(index: Int, value: A) = (_numRows, _numCols) match {
    case (1, _) => _array(0)(index) = value
    case (_, 1) => _array(index)(0) = value
    case _ => throw new IllegalArgumentException("Cannot assign row of matrix to single value")
  }

  private def scalarOp(f: A => A): Matrix[A] = {
    val result = Matrix[A](_numRows, _numCols)
    for (i <- 0 until _numRows; j <- 0 until _numCols) {
      result(i, j) = f(this(i, j))
    }

    result
  }

  private def opByEntry(f: (A, A) => A)(that: Matrix[A]): Matrix[A] = {
    require(_numRows == that.rows && _numCols == that.cols,
      "Cannot operate element-wise on matrices of unequal sizes")

    val result = Matrix[A](_numRows, _numCols)
    for (i <- 0 until _numRows; j <- 0 until _numCols) {
      result(i, j) = f(this(i, j), that(i, j))
    }

    result
  }

  def toSBitstream: Matrix[SBitstream] = {
    val result = Matrix[SBitstream](_numRows, _numCols)

    for (i <- 0 until _numRows; j <- 0 until _numCols) {
      result(i, j) = SBitstream(this(i, j).toDouble)
    }

    result
  }

  def toDouble: Matrix[Double] = {
    val result = Matrix[Double](_numRows, _numCols)

    for (i <- 0 until _numRows; j <- 0 until _numCols) {
      result(i, j) = this(i, j).toDouble
    }

    result
  }

  def T: Matrix[A] = {
    val result = Matrix[A](_numCols, _numRows)
    for (i <- 0 until _numCols; j <- 0 until _numRows) {
      result(i, j) = this(j, i)
    }

    result
  }

  def ==(that: Matrix[A]): Boolean = {
    require(_numRows == that.rows && _numCols == that.cols,
      "Cannot compare matrices of unequal sizes")

    var retVal = true
    for (i <- 0 until _numRows; j <- 0 until _numCols) {
      if (this(i, j) != that(i, j)) retVal = false
    }

    retVal
  }
  def !=(that: Matrix[A]): Boolean = !(this == that)

  def +(that: A): Matrix[A] = scalarOp(num.plus(_, that))
  def -(that: A): Matrix[A] = scalarOp(num.minus(_, that))
  def *(that: A): Matrix[A] = scalarOp(num.times(_, that))
  def /(that: A)(implicit numMatrix: MatrixSpecificType[A]): Matrix[A]
  	= scalarOp(numMatrix.div(_, that))

  def +(that: Matrix[A]): Matrix[A] = this.opByEntry(num.plus)(that)
  def -(that: Matrix[A]): Matrix[A] = this.opByEntry(num.minus)(that)
  def **(that: Matrix[A]): Matrix[A] = this.opByEntry(num.times)(that)
  def /(that: Matrix[A])(implicit numMatrix: MatrixSpecificType[A]): Matrix[A]
    = this.opByEntry(numMatrix.div)(that)

  def *(that: Matrix[A]): Matrix[A] = {
    require(_numCols == that.rows, "Cannot multiply matrices of mismatched inner dimension")

    val result = Matrix[A](_numRows, that.cols)
    for (i <- 0 until _numRows; j <- 0 until that.cols) {
      var accum = num.zero
      for (k <- 0 until _numCols) {
        val prod = num.times(this(i, k), that(k, j))
        accum = num.plus(accum, prod)
      }
      result(i, j) = accum
    }

    result
  }

  def dot(that: Matrix[A]): A = {
    var result = Matrix[A](1, 1)
    (_numRows, _numCols, that.rows, that.cols) match {
      case (1, _, 1, _) => result = this * that.T
      case (_, 1, 1, _) => result = this.T * that.T
      case (_, 1, _, 1) => result = this.T * that
      case (1, _, _, 1) => result = this * that
      case _ => throw new IllegalArgumentException("Cannot take the dot product of a matrix")
    }

    result(0, 0)
  }

  def cross(that: Matrix[A]): Matrix[A] = {
    var result = Matrix[A](_numRows, _numCols)

    var body = (x: Int, y: Int) => {
      if (x != y)
        throw new IllegalArgumentException("Cannot take cross product of vectors with mismatched length")
      else {
        result(0) = this(1) * that(2) - this(2) * that(1)
        result(1) = this(0) * that(2) - this(2) * that(0)
        result(2) = this(0) * that(1) - this(1) * that(0)
      }
    }

    (_numRows, _numCols, that.rows, that.cols) match {
      case (1, x, 1, y) => body(x, y)
      case (x, 1, 1, y) => body(x, y)
      case (x, 1, y, 1) => body(x, y)
      case (1, x, y, 1) => body(x, y)
      case _ => throw new IllegalArgumentException("Cannot take the cross product of a matrix")
    }

    result
  }

  def mkString: String = {
    var str = "\n"

    for (i <- 0 until _numRows) {
      str = str + "    |\t"
      for (j <- 0 until _numCols) {
        if (this(i, j).toDouble >= 0)
          str = str + f" ${this(i, j).toDouble}%.4f\t"
        else
          str = str + f"${this(i, j).toDouble}%.4f\t"
      }
      str = str + "|\n"
    }

    str
  }

  def mkStringCsv: String = {
    var str = "\n"

    for (i <- 0 until _numRows) {
      // str = str + "    |\t"
      for (j <- 0 until _numCols-1) {
        str = str + f"${this(i, j).toDouble}%.4f,\t"
      }
      str = str + f"${this(i, _numCols-1).toDouble}%.4f\t"
      str = str + "\n"
    }

    str
  }

}

object Matrix {

  def apply[A: ClassTag](array: Array[Array[A]])(implicit num: Numeric[A]): Matrix[A] = {
    val rows = array.length
    val cols = array(0).length
    require(array.forall(_.length == cols), "Rows of array must be same length")

    var result = Matrix[A](rows, cols)
    for (i <- 0 until rows; j <- 0 until cols)
      result(i, j) = array(i)(j)

    result
  }

  def zeros[A: ClassTag](rows: Int, cols: Int)(implicit num: Numeric[A]): Matrix[A] = {
    val result = Matrix[A](rows, cols)

    for (i <- 0 until rows; j <- 0 until cols) {
      result(i, j) = num.zero
    }

    result
  }

  def ones[A: ClassTag](rows: Int, cols: Int)(implicit num: Numeric[A]): Matrix[A] = {
    val result = Matrix[A](rows, cols)

    for (i <- 0 until rows; j <- 0 until cols) {
      result(i, j) = num.one
    }

    result
  }

  def eye[A: ClassTag](rows: Int, cols: Int)(implicit num: Numeric[A]): Matrix[A] = {
    val result = Matrix[A](rows, cols)

    for (i <- 0 until rows; j <- 0 until cols) {
      if (i == j) result(i, j) = num.one
      else result(i, j) = num.zero
    }

    result
  }

  def sqrt[A: ClassTag](m: Matrix[A])
                       (implicit num: Numeric[A], numMatrix: MatrixSpecificType[A]): Matrix[A]
    = m.scalarOp(numMatrix.sqrt)

  def norm[A: TypeTag](m: Matrix[A], normType: String = "L2")
                      (implicit num: Numeric[A], numMatrix: MatrixSpecificType[A]): Double = {

    val l1Body = (x: A, y: A) => {
      val yAbs = num.abs(y)

      num.plus(x, yAbs)
    }
    val l2Body = (x: A, y: A) => {
      val ySq = num.times(y, y)

      num.plus(x, ySq)
    }

    normType match {
      case "L1" => typeOf[A] match {
        case t if t =:= typeOf[SBitstream] =>
          throw new NotImplementedError("No support for L1 norm of a Bitstream")
        case _ => m.T._array.map(_.foldLeft(num.zero)(l1Body)).max.toDouble
      }
      case "L2" => (m.rows, m.cols) match {
        case (_, 1) | (1, _) =>
          numMatrix.sqrtDouble(
          	m._array.map(_.foldLeft(num.zero)(l2Body)).foldLeft(num.zero)(num.plus))
        case _ => throw new NotImplementedError("No support for L2 norm of a Matrix")
      }
      case "inf" => typeOf[A] match {
        case t if t =:= typeOf[SBitstream] =>
          throw new NotImplementedError("No support for inf norm of Bitstream")
        case _ => m._array.map(_.foldLeft(num.zero)(l1Body)).max.toDouble
      }
      case "fro" => typeOf[A] match {
        case t if t =:= typeOf[SBitstream] =>
          throw new NotImplementedError("No support for fro norm of Bitstream")
        case _ =>
          numMatrix.sqrtDouble(
          	m._array.map(_.foldLeft(num.zero)(l2Body)).foldLeft(num.zero)(num.plus))
      }
      case _ =>
      	throw new IllegalArgumentException(s"Provided matrix norm type ($normType) is invalid")
    }
  }

  def norm(m: Matrix[SBitstream]): SBitstream = {
    val l2Body = (x: SBitstream, y: SBitstream) => x + (y * y)

    (m.rows, m.cols) match {
      case (_, 1) | (1, _) =>
        SBitstream.sqrt(
          m._array.map(_.foldLeft(SBitstream(0))(l2Body)).foldLeft(SBitstream(0))(_+_))
      case _ => throw new NotImplementedError("No support for L2 norm of a Matrix")
    }
  }

  def rand[A: ClassTag](rows: Int, cols: Int)
                       (implicit num: Numeric[A], numMatrix: MatrixSpecificType[A]): Matrix[A] = {

    val result = Matrix[A](rows, cols)

    for (i <- 0 until rows; j <- 0 until cols) {
      result(i, j) = numMatrix.rand()
    }

    result
  }

  def conv2d[A: ClassTag](m: Matrix[A], k: Matrix[A])(implicit num: Numeric[A]): Matrix[A] = {
    var result = Matrix[A](m.rows, m.cols)

    for (i <- 0 until m.rows; j <- 0 until m.cols) {
      var accum = num.zero
      for (p <- 0 until k.rows; q <- 0 until k.cols) {
        val x = i + (p - k.rows / 2)
        val y = j + (q - k.cols / 2)
        accum = if (x >= 0 && y >= 0) accum + m(x, y) * k(p, q) else accum
      }
      result(i, j) = accum
    }

    result
  }

  def reshape[A: ClassTag](m: Matrix[A], rows: Int, cols: Int)(implicit num: Numeric[A]):
      Matrix[A] = {

    var result = Matrix[A](rows, cols)

    var ii = 0
    var jj = 0
    for (i <- 0 until m.rows) {
      for (j <- 0 until m.cols) {
        result(ii, jj) = m(i, j)
        jj = (jj + 1) % cols
        ii = if (jj == 0) ii + 1 else ii
      }
    }

    result
  }

  def horzConcat[A: ClassTag](m: Matrix[A], n: Matrix[A])(implicit num: Numeric[A]): Matrix[A] = {
    require(m.rows == n.rows, "Can't horizontally concatenate matrices with different # of rows")
    var result = Matrix[A](m.rows, m.cols + n.cols)

    for (i <- 0 until m.rows) {
      for (j <- 0 until m.cols) {
        result(i, j) = m(i, j)
      }

      for (j <- m.cols until m.cols + n.cols) {
        result(i, j) = n(i, j - m.cols)
      }
    }

    result
  }

  def vertConcat[A: ClassTag](m: Matrix[A], n: Matrix[A])(implicit num: Numeric[A]): Matrix[A] = {
    require(m.cols == n.cols, "Can't vertically concatenate matrices with different # of columns")
    var result = Matrix[A](m.rows + n.cols, m.cols)

    for (j <- 0 until m.cols) {
      for (i <- 0 until m.rows) {
        result(i, j) = m(i, j)
      }

      for (i <- m.rows until m.rows + n.rows) {
        result(i, j) = n(i - m.rows, j)
      }
    }

    result
  }

  def tile[A: ClassTag](m: Matrix[A], numTiles: Tuple2[Int, Int])(implicit num: Numeric[A]):
      Array[Array[Matrix[A]]] = {

    val xTileSize = math.ceil(m.rows.toDouble / numTiles._1.toDouble).toInt
    val yTileSize = math.ceil(m.cols.toDouble / numTiles._2.toDouble).toInt

    var tiles = Array.ofDim[Matrix[A]](numTiles._1, numTiles._2)
    for (i <- 0 until numTiles._1; j <- 0 until numTiles._2) {
      tiles(i)(j) = Matrix.zeros[A](xTileSize, yTileSize)
      for (k <- 0 until xTileSize; l <- 0 until yTileSize) {
        val x = min(i * xTileSize + k, m.rows - 1)
        val y = min(j * yTileSize + l, m.cols - 1)
        tiles(i)(j)(k, l) = m(x, y)
      }
    }

    tiles
  }

  implicit class MatrixOps[A](lhs: A)
  							 (implicit num: Numeric[A], numMatrix: MatrixSpecificType[A]) {
    def +(that: Matrix[A]): Matrix[A] = that.scalarOp(num.plus(lhs, _))
    def -(that: Matrix[A]): Matrix[A] = that.scalarOp(num.minus(lhs, _))
    def *(that: Matrix[A]): Matrix[A] = that.scalarOp(num.times(lhs, _))
    def /(that: Matrix[A]): Matrix[A] = that.scalarOp(numMatrix.div(lhs, _))
  }

  implicit class MatrixIntOps(lhs: Int) {
    def +(that: Matrix[Double]): Matrix[Double] = that.scalarOp(lhs.toDouble + _)
    def -(that: Matrix[Double]): Matrix[Double] = that.scalarOp(lhs.toDouble - _)
    def *(that: Matrix[Double]): Matrix[Double] = that.scalarOp(lhs.toDouble * _)
    def /(that: Matrix[Double]): Matrix[Double] = that.scalarOp(lhs.toDouble / _)
  }

  implicit class MatrixDoubleOps(lhs: Double) {
    def +(that: Matrix[Double]): Matrix[Double] = that.scalarOp(lhs + _)
    def -(that: Matrix[Double]): Matrix[Double] = that.scalarOp(lhs - _)
    def *(that: Matrix[Double]): Matrix[Double] = that.scalarOp(lhs * _)
    def /(that: Matrix[Double]): Matrix[Double] = that.scalarOp(lhs / _)
  }

  implicit class MatrixIntSBitstreamOps(lhs: Int) {
    def +(that: Matrix[SBitstream]): Matrix[SBitstream] = that.scalarOp(lhs + _)
    def -(that: Matrix[SBitstream]): Matrix[SBitstream] = that.scalarOp(lhs - _)
    def *(that: Matrix[SBitstream]): Matrix[SBitstream] = that.scalarOp(lhs * _)
    def /(that: Matrix[SBitstream]): Matrix[SBitstream] = that.scalarOp(lhs / _)
  }

  implicit class MatrixDoubleSBitstreamIntOps(lhs: Double) {
    def +(that: Matrix[SBitstream]): Matrix[SBitstream] = that.scalarOp(lhs + _)
    def -(that: Matrix[SBitstream]): Matrix[SBitstream] = that.scalarOp(lhs - _)
    def *(that: Matrix[SBitstream]): Matrix[SBitstream] = that.scalarOp(lhs * _)
    def /(that: Matrix[SBitstream]): Matrix[SBitstream] = that.scalarOp(lhs / _)
  }

  implicit class MatrixFixedGainDiv(lhs: Matrix[SBitstream]) {
    def :/(that: Int): Matrix[SBitstream] = lhs.scalarOp(_ :/ that)
    def :/(that: Long): Matrix[SBitstream] = lhs.scalarOp(_ :/ that)
    def :/(that: Double): Matrix[SBitstream] = lhs.scalarOp(_ :/ that)
  }

  // implicit class MatrixSBitstreamOps(lhs: Matrix[SBitstream]) {
  //   def pop: (Matrix[Int], Matrix[Int]) = {
  //     var resultP = Matrix.zeros[Int](lhs.rows, lhs.cols)
  //     var resultN = Matrix.zeros[Int](lhs.rows, lhs.cols)

  //     for (i <- 0 until lhs.rows; j <- 0 until lhs.cols) {
  //       lhs(i, j).pop match {
  //         case (pos, neg) => {
  //           resultP(i, j) = pos
  //           resultN(i, j) = neg
  //         }
  //       }
  //     }

  //     (resultP, resultN)
  //   }

  //   def push(x: Tuple2[Matrix[Int], Matrix[Int]]): Unit = {
  //     for (i <- 0 until lhs.rows; j <- 0 until lhs.cols) {
  //       lhs(i, j).push((x._1(i, j), x._2(i, j)))
  //     }
  //   }
  // }

}