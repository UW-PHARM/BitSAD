package bitstream.types

import math._
import math.Numeric.Implicits._
import math.Numeric._
import Array._
import reflect._
import scala.reflect.runtime.universe._
import scala.util.Random
import bitstream.simulator.internal.units._

// trait MatrixSpecificType[A] {
//   def id: Function1[A, Int]
//   def setId: Function2[A, Int, Unit]
//   def div: Function2[A, A, A]
//   def sqrt: Function1[A, A]
//   def sqrtDouble: Function1[A, Double]
//   def sqrtBS: Function1[A, Bitstream]
//   def rand: Function0[A]
// }

// object MatrixSpecificType {
//   implicit object MatrixInt extends MatrixSpecificType[Int] {
//     def id = (x: Int) => 0
//     def setId = (x: Int, id: Int) => {}
//     def div = _/_
//     def sqrt = math.sqrt(_).toInt
//     def sqrtDouble = math.sqrt(_)
//     def sqrtBS = (n: Int) => SBitstream(math.sqrt(n))
//     def rand = () => math.random().toInt
//   }

//   implicit object MatrixLong extends MatrixSpecificType[Long] {
//     def id = (x: Long) => 0
//     def setId = (x: Long, id: Int) => {}
//     def div = _/_
//     def sqrt = math.sqrt(_).toLong
//     def sqrtDouble = math.sqrt(_)
//     def sqrtBS = (n: Long) => SBitstream(math.sqrt(n))
//     def rand = () => math.random().toLong
//   }

//   implicit object MatrixDouble extends MatrixSpecificType[Double] {
//     def id = (x: Double) => 0
//     def setId = (x: Double, id: Int) => {}
//     def div = _/_
//     def sqrt = math.sqrt
//     def sqrtDouble = math.sqrt
//     def sqrtBS = (n: Double) => SBitstream(math.sqrt(n))
//     def rand = math.random
//   }

//   implicit object MatrixBitstream extends MatrixSpecificType[SBitstream] {
//     def id = _.id
//     def setId = (x: SBitstream, id: Int) => x.id = id
//     def div = _/_
//     def sqrt = SBitstream.sqrt
//     def sqrtDouble = SBitstream.sqrt(_).value
//     def sqrtBS = SBitstream.sqrt
//     def rand = () => SBitstream(math.random())
//   }
// }

case class Matrix[A: ClassTag] (val _numRows:Int, val _numCols:Int)(implicit num: SimulatableNumeric[A]) {

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

  private def scalarOp(f: A => (String, String) => A)
                      (xName: String, yName: String): Matrix[A] = {
    val result = Matrix[A](_numRows, _numCols)
    for (i <- 0 until _numRows; j <- 0 until _numCols) {
      result(i, j) = f(this(i, j))(s"$xName($i, $j)", s"$yName($i, $j)")
    }

    result
  }

  private def scalarOp(f: A => SimulationId => A)(id: SimulationId): Matrix[A] = {
    val result = Matrix[A](_numRows, _numCols)
    for (i <- 0 until _numRows; j <- 0 until _numCols) {
      result(i, j) = f(this(i, j))(SimulationId(s"${id.lhs}($i, $j)", id.rhs))
    }

    result
  }

  private def opByEntry(f: (A, A) => (String, String) => A)(that: Matrix[A])
                       (xName: String, yName: String): Matrix[A] = {
    require(_numRows == that.rows && _numCols == that.cols,
      "Cannot operate element-wise on matrices of unequal sizes")

    val result = Matrix[A](_numRows, _numCols)
    for (i <- 0 until _numRows; j <- 0 until _numCols) {
      result(i, j) = f(this(i, j), that(i, j))(s"$xName($i, $j)", s"$yName($i, $j)")
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

  def toArray: Array[Array[A]] = _array

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

  def +(that: A)(implicit id: SimulationId): Matrix[A]
    = scalarOp((x: A) => (y: String, z: String) => num.plus(x, that)(y, z))(id.lhs, id.rhs)
  def -(that: A)(implicit id: SimulationId): Matrix[A]
    = scalarOp((x: A) => (y: String, z: String) => num.minus(x, that)(y, z))(id.lhs, id.rhs)
  def *(that: A)(implicit id: SimulationId): Matrix[A]
    = scalarOp((x: A) => (y: String, z: String) => num.times(x, that)(y, z))(id.lhs, id.rhs)
  def /(that: A)(implicit id: SimulationId): Matrix[A]
    = scalarOp((x: A) => (y: String, z: String) => num.div(x, that)(y, z))(id.lhs, id.rhs)

  def +(that: Matrix[A])(implicit id: SimulationId): Matrix[A]
    = this.opByEntry(num.plus _)(that)(id.lhs, id.rhs)
  def -(that: Matrix[A])(implicit id: SimulationId): Matrix[A]
    = this.opByEntry(num.minus _)(that)(id.lhs, id.rhs)
  def **(that: Matrix[A])(implicit id: SimulationId): Matrix[A]
    = this.opByEntry(num.times _)(that)(id.lhs, id.rhs)
  def /(that: Matrix[A])(implicit id: SimulationId): Matrix[A]
    = this.opByEntry(num.div _)(that)(id.lhs, id.rhs)

  def *(that: Matrix[A])(implicit id: SimulationId): Matrix[A] = {
    require(_numCols == that.rows, "Cannot multiply matrices of mismatched inner dimension")

    if (id.lhs == "" && id.rhs == "") {
      val result = Matrix[A](_numRows, that.cols)
      for (i <- 0 until _numRows; j <- 0 until that.cols) {
        var accum = num.zero
        for (k <- 0 until _numCols) {
          val prod = num.times(this(i, k), that(k, j))(id.lhs, id.rhs)
          accum = num.plus(accum, prod)
        }
        result(i, j) = accum
      }

      result
    } else {
      var result = (this * that)(SimulationId("", "")).toSBitstream
      var op = Matrix.findOperator(id.lhs, id.rhs, "times", result.rows, result.cols)
      result.push(op.evaluate(List(this.asInstanceOf[Matrix[SBitstream]].pop,
                                   that.asInstanceOf[Matrix[SBitstream]].pop)))

      result.asInstanceOf[Matrix[A]]
    }
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

  private var opMap: Map[Tuple3[String, String, String], MatrixOperator] = Map()

  def findOperator(xId: String, yId: String, op: String, rows: Int, cols: Int): MatrixOperator = {
    if (opMap contains (xId, yId, op)) opMap((xId, yId, op))
    else op match {
      case "times" => {
        var multiplier = new SCSignedMatrixMultiplier(rows, cols)
        opMap += ((xId, yId, "times") -> multiplier)
        opMap((xId, yId, op))
      }
      case "norm" => {
        var normer = new SCL2Norm(rows)
        opMap += ((xId, yId, "norm") -> normer)
        opMap((xId, yId, op))
      }
    }
  }

  // private def mkIdResult[A: TypeTag](result: Matrix[A], op: String, xId: Int, yId: Int)
  //                                   (implicit numMatrix: MatrixSpecificType[A]):
  //     Matrix[A] = {
  //   if (typeOf[A] =:= typeOf[SBitstream]) {
  //     var (_, id) = Matrix.findOperator(xId, yId, op, result.rows, result.cols)
  //     result.id = id
  //     if (Matrix.idMap contains id) {
  //       val subIds = Matrix.idMap(id)
  //       for (i <- 0 until result.rows; j <- 0 until result.cols) {
  //         numMatrix.setId(result(i, j), subIds(i)(j))
  //       }
  //     } else {
  //       var subIds = Array.ofDim[Int](result.rows, result.cols)
  //       for (i <- 0 until result.rows; j <- 0 until result.cols) {
  //         subIds(i)(j) = numMatrix.id(result(i, j))
  //       }
  //       Matrix.idMap += (id -> subIds)
  //     }

  //     result
  //   } else result
  // }

  // private def mkLScalarIdResult[A: TypeTag](result: Matrix[A], op: String,
  //                                            scalar: A, yId: Int)
  //                                           (implicit num: Numeric[A],
  //                                            numMatrix: MatrixSpecificType[A]):
  //     Matrix[A] = {
  //   var xId =
  //     if (typeOf[A] =:= typeOf[SBitstream]) numMatrix.id(scalar)
  //     else math.round(scalar.toDouble).toInt

  //   mkIdResult(result, op, xId, yId)
  // }

  // private def mkRScalarIdResult[A: TypeTag](result: Matrix[A], op: String,
  //                                            xId: Int, scalar: A)
  //                                           (implicit num: Numeric[A],
  //                                            numMatrix: MatrixSpecificType[A]):
  //     Matrix[A] = {
  //   var yId =
  //     if (typeOf[A] =:= typeOf[SBitstream]) numMatrix.id(scalar)
  //     else math.round(scalar.toDouble).toInt

  //   mkIdResult(result, op, xId, yId)
  // }

  def apply[A: ClassTag](array: Array[Array[A]])(implicit num: SimulatableNumeric[A]): Matrix[A] = {
    val rows = array.length
    val cols = array(0).length
    require(array.forall(_.length == cols), "Rows of array must be same length")

    var result = Matrix[A](rows, cols)
    for (i <- 0 until rows; j <- 0 until cols)
      result(i, j) = array(i)(j)

    result
  }

  def zeros[A: ClassTag](rows: Int, cols: Int)(implicit num: SimulatableNumeric[A]): Matrix[A] = {
    val result = Matrix[A](rows, cols)

    for (i <- 0 until rows; j <- 0 until cols) {
      result(i, j) = num.zero
    }

    result
  }

  def ones[A: ClassTag](rows: Int, cols: Int)(implicit num: SimulatableNumeric[A]): Matrix[A] = {
    val result = Matrix[A](rows, cols)

    for (i <- 0 until rows; j <- 0 until cols) {
      result(i, j) = num.one
    }

    result
  }

  def eye[A: ClassTag](rows: Int, cols: Int)(implicit num: SimulatableNumeric[A]): Matrix[A] = {
    val result = Matrix[A](rows, cols)

    for (i <- 0 until rows; j <- 0 until cols) {
      if (i == j) result(i, j) = num.one
      else result(i, j) = num.zero
    }

    result
  }

  def sqrt[A: ClassTag](m: Matrix[A])(implicit id: SimulationId,
                                      num: SimulatableNumeric[A]): Matrix[A]
    = m.scalarOp((x: A) => (y: String, z: String) => num.sqrt(x)(y, z))(id.lhs, id.rhs)

  def norm[A: TypeTag](m: Matrix[A], normType: String = "L2")
                      (implicit num: SimulatableNumeric[A]): Double = {
    val l1Body = (x: A, y: A) => {
      val yAbs = num.abs(y)

      num.plus(x, yAbs)("", "")
    }
    val l2Body = (x: A, y: A) => {
      val ySq = num.times(y, y)("", "")

      num.plus(x, ySq)("", "")
    }

    normType match {
      case "L1" => typeOf[A] match {
        case t if t =:= typeOf[SBitstream] =>
          throw new NotImplementedError("No support for L1 norm of a Bitstream")
        case _ => m.T._array.map(_.foldLeft(num.zero)(l1Body)).max.toDouble
      }
      case "L2" => (m.rows, m.cols) match {
        case (_, 1) | (1, _) =>
          num.sqrtDouble(
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
          num.sqrtDouble(
          	m._array.map(_.foldLeft(num.zero)(l2Body)).foldLeft(num.zero)(num.plus))
      }
      case _ =>
      	throw new IllegalArgumentException(s"Provided matrix norm type ($normType) is invalid")
    }
  }

  def norm(m: Matrix[SBitstream])(implicit id: SimulationId):
      SBitstream = {
    (m.rows, m.cols) match {
      case (_, 1) => {
        var result = SBitstream(norm(m.toDouble))
        var op = findOperator(id.lhs, id.rhs, "norm", m.rows, m.cols)
        val bit = op.evaluate(List(m.pop))
        result.push((bit._1(0, 0), bit._2(0, 0)))
        result
      }
      case (1, _) =>
        throw new IllegalArgumentException("L2 norm of Matrix[SBitstream] requires column vector")
      case _ => throw new NotImplementedError("No support for L2 norm of a Matrix")
    }
  }

  def rand[A: ClassTag](rows: Int, cols: Int)(implicit num: SimulatableNumeric[A]): Matrix[A] = {
    val result = Matrix[A](rows, cols)

    for (i <- 0 until rows; j <- 0 until cols) {
      result(i, j) = num.rand()
    }

    result
  }

  def conv2d[A: ClassTag](m: Matrix[A], k: Matrix[A])(implicit num: SimulatableNumeric[A]):
      Matrix[A] = {
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

  def reshape[A: ClassTag](m: Matrix[A], rows: Int, cols: Int)(implicit num: SimulatableNumeric[A]):
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

  def horzConcat[A: ClassTag](m: Matrix[A], n: Matrix[A])(implicit num: SimulatableNumeric[A]): Matrix[A] = {
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

  def vertConcat[A: ClassTag](m: Matrix[A], n: Matrix[A])(implicit num: SimulatableNumeric[A]): Matrix[A] = {
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

  def tile[A: ClassTag](m: Matrix[A], numTiles: Tuple2[Int, Int])(implicit num: SimulatableNumeric[A]):
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

  implicit class MatrixOps[A](lhs: A)(implicit num: SimulatableNumeric[A]) {
    def +(that: Matrix[A])(implicit id: SimulationId): Matrix[A]
      = that.scalarOp((x: A) => (y: String, z: String) => num.plus(lhs, x)(y, z))(id.lhs, id.rhs)
    def -(that: Matrix[A])(implicit id: SimulationId): Matrix[A]
      = that.scalarOp((x: A) => (y: String, z: String) => num.minus(lhs, x)(y, z))(id.lhs, id.rhs)
    def *(that: Matrix[A])(implicit id: SimulationId): Matrix[A]
      = that.scalarOp((x: A) => (y: String, z: String) => num.times(lhs, x)(y, z))(id.lhs, id.rhs)
    def /(that: Matrix[A])(implicit id: SimulationId): Matrix[A]
      = that.scalarOp((x: A) => (y: String, z: String) => num.div(lhs, x)(y, z))(id.lhs, id.rhs)
  }

  implicit class MatrixIntOps(lhs: Int)(implicit num: SimulatableNumeric[Double]) {
    def +(that: Matrix[Double])(implicit id: SimulationId): Matrix[Double]
      = that.scalarOp((x: Double) => (y: String, z: String) => num.plus(lhs.toDouble, x)(y, z))(id.lhs, id.rhs)
    def -(that: Matrix[Double])(implicit id: SimulationId): Matrix[Double]
      = that.scalarOp((x: Double) => (y: String, z: String) => num.minus(lhs.toDouble, x)(y, z))(id.lhs, id.rhs)
    def *(that: Matrix[Double])(implicit id: SimulationId): Matrix[Double]
      = that.scalarOp((x: Double) => (y: String, z: String) => num.times(lhs.toDouble, x)(y, z))(id.lhs, id.rhs)
    def /(that: Matrix[Double])(implicit id: SimulationId): Matrix[Double]
      = that.scalarOp((x: Double) => (y: String, z: String) => num.div(lhs.toDouble, x)(y, z))(id.lhs, id.rhs)
  }

  implicit class MatrixDoubleOps(lhs: Double)(implicit num: SimulatableNumeric[Double]) {
    def +(that: Matrix[Double])(implicit id: SimulationId): Matrix[Double]
      = that.scalarOp((x: Double) => (y: String, z: String) => num.plus(lhs, x)(y, z))(id.lhs, id.rhs)
    def -(that: Matrix[Double])(implicit id: SimulationId): Matrix[Double]
      = that.scalarOp((x: Double) => (y: String, z: String) => num.minus(lhs, x)(y, z))(id.lhs, id.rhs)
    def *(that: Matrix[Double])(implicit id: SimulationId): Matrix[Double]
      = that.scalarOp((x: Double) => (y: String, z: String) => num.times(lhs, x)(y, z))(id.lhs, id.rhs)
    def /(that: Matrix[Double])(implicit id: SimulationId): Matrix[Double]
      = that.scalarOp((x: Double) => (y: String, z: String) => num.div(lhs, x)(y, z))(id.lhs, id.rhs)
  }

  implicit class MatrixIntSBitstreamOps(lhs: Int)(implicit num: SimulatableNumeric[SBitstream]) {
    def +(that: Matrix[SBitstream])(implicit id: SimulationId): Matrix[SBitstream]
      = that.scalarOp((x: SBitstream) => (y: String, z: String) => num.plus(SBitstream(lhs), x)(y, z))(id.lhs, id.rhs)
    def -(that: Matrix[SBitstream])(implicit id: SimulationId): Matrix[SBitstream]
      = that.scalarOp((x: SBitstream) => (y: String, z: String) => num.minus(SBitstream(lhs), x)(y, z))(id.lhs, id.rhs)
    def *(that: Matrix[SBitstream])(implicit id: SimulationId): Matrix[SBitstream]
      = that.scalarOp((x: SBitstream) => (y: String, z: String) => num.times(SBitstream(lhs), x)(y, z))(id.lhs, id.rhs)
    def /(that: Matrix[SBitstream])(implicit id: SimulationId): Matrix[SBitstream]
      = that.scalarOp((x: SBitstream) => (y: String, z: String) => num.div(SBitstream(lhs), x)(y, z))(id.lhs, id.rhs)
  }

  implicit class MatrixDoubleSBitstreamIntOps(lhs: Double)(implicit num: SimulatableNumeric[SBitstream]) {
    def +(that: Matrix[SBitstream])(implicit id: SimulationId): Matrix[SBitstream]
      = that.scalarOp((x: SBitstream) => (y: String, z: String) => num.plus(SBitstream(lhs), x)(y, z))(id.lhs, id.rhs)
    def -(that: Matrix[SBitstream])(implicit id: SimulationId): Matrix[SBitstream]
      = that.scalarOp((x: SBitstream) => (y: String, z: String) => num.minus(SBitstream(lhs), x)(y, z))(id.lhs, id.rhs)
    def *(that: Matrix[SBitstream])(implicit id: SimulationId): Matrix[SBitstream]
      = that.scalarOp((x: SBitstream) => (y: String, z: String) => num.times(SBitstream(lhs), x)(y, z))(id.lhs, id.rhs)
    def /(that: Matrix[SBitstream])(implicit id: SimulationId): Matrix[SBitstream]
      = that.scalarOp((x: SBitstream) => (y: String, z: String) => num.div(SBitstream(lhs), x)(y, z))(id.lhs, id.rhs)
  }

  implicit class MatrixFixedGainDiv(lhs: Matrix[SBitstream]) {
    def :/(that: Int)(implicit id: SimulationId): Matrix[SBitstream]
      = lhs.scalarOp((x: SBitstream) => (y: SimulationId) => (x :/ that)(y))(id)
    def :/(that: Long)(implicit id: SimulationId): Matrix[SBitstream]
      = lhs.scalarOp((x: SBitstream) => (y: SimulationId) => (x :/ that)(y))(id)
    def :/(that: Double)(implicit id: SimulationId): Matrix[SBitstream]
      = lhs.scalarOp((x: SBitstream) => (y: SimulationId) => (x :/ that)(y))(id)
  }

  implicit class MatrixSBitstreamBitLevelOps(lhs: Matrix[SBitstream]) {
    def push(bits: Tuple2[Matrix[Int], Matrix[Int]]): Unit = {
      for (i <- 0 until lhs.rows; j <- 0 until lhs.cols) {
        val pBit = bits._1(i, j)
        val nBit = bits._2(i, j)
        lhs(i, j).push((pBit, nBit))
      }
    }

    def pop: Tuple2[Matrix[Int], Matrix[Int]] = {
      var pResult = Matrix.zeros[Int](lhs.rows, lhs.cols)
      var nResult = Matrix.zeros[Int](lhs.rows, lhs.cols)

      for (i <- 0 until lhs.rows; j <- 0 until lhs.cols) {
        val bit = lhs(i, j).pop
        pResult(i, j) = bit._1
        nResult(i, j) = bit._2
      }

      (pResult, nResult)
    }

    def observe: Tuple2[Matrix[Int], Matrix[Int]] = {
      var pResult = Matrix.zeros[Int](lhs.rows, lhs.cols)
      var nResult = Matrix.zeros[Int](lhs.rows, lhs.cols)

      for (i <- 0 until lhs.rows; j <- 0 until lhs.cols) {
        val bit = lhs(i, j).observe
        pResult(i, j) = bit._1
        nResult(i, j) = bit._2
      }

      (pResult, nResult)
    }
  }

}