package bitstream.simulator.internal.units

import math._
import scala.util.Random
import bitstream.simulator.units.Decorrelator
import bitstream.types.Matrix
import bitstream.types.SimulatableNumeric._

trait Operator {
  // Stub for polymorphism
  def evaluate(inputs: List[Tuple2[Int, Int]]): (Int, Int) = ???
}

trait MatrixOperator {
  // Stub for polymorphism
  def evaluate(inputs: List[Tuple2[Matrix[Int], Matrix[Int]]]):
    (Matrix[Int], Matrix[Int]) = ???
}

class DummyMatrixOperator() extends MatrixOperator {
  override def evaluate(inputs: List[Tuple2[Matrix[Int], Matrix[Int]]]):
      (Matrix[Int], Matrix[Int]) =
    throw new NotImplementedError("The DummyMatrixOperator class is not callable.")
}

class SCAdder() {

  private var counter = 0

  def evaluate(a: Int, b: Int): Int = {
    // Increment counter
    counter += a + b

    // Decide output
    val c = if (counter >= 1) 1 else 0

    // Decrement counter
    counter = math.max(counter - c, 0)

    c
  }

}

class SCMatrixAdder(numRows: Int, numCols: Int) {

  private var adders = Array.ofDim[SCAdder](numRows, numCols)

  for (i <- 0 until numRows; j <- 0 until numCols) {
    adders(i)(j) = new SCAdder()
  }

  def evaluate(A: Matrix[Int], B: Matrix[Int]): Matrix[Int] = {
    var C = Matrix.zeros[Int](numRows, numCols)

    for (i <- 0 until numRows; j <- 0 until numCols) {
      C(i, j) = adders(i)(j).evaluate(A(i, j), B(i, j))
    }

    C
  }

}

class SCSignedAdder() extends Operator {

  private var pAdder = new SCAdder()
  private var nAdder = new SCAdder()

  override def evaluate(inputs: List[Tuple2[Int, Int]]): (Int, Int) = {
    val a = inputs(0)
    val b = inputs(1)

    val c1 = pAdder.evaluate(a._1, b._1)
    val c2 = nAdder.evaluate(a._2, b._2)

    (c1, c2)
  }

}

class SCSaturatingSubtractor() {

  private var counter = 0

  def evaluate(a: Int, b: Int): Int = {
    // Increment counter
    counter += (a ^ b) & b

    // Decrement counters
    counter = math.max(counter - ((a ^ b) & a), 0)

    // Decide output
    val c = if (counter == 0 && a == 1 && b == 0) 1 else 0

    c
  }

}

class SCMatrixSaturatingSubtractor(numRows: Int, numCols: Int) {

  private var subs = Array.ofDim[SCSaturatingSubtractor](numRows, numCols)

  for (i <- 0 until numRows; j <- 0 until numCols) {
    subs(i)(j) = new SCSaturatingSubtractor()
  }

  def evaluate(A: Matrix[Int], B: Matrix[Int]): Matrix[Int] = {
    var C = Matrix.zeros[Int](numRows, numCols)

    for (i <- 0 until numRows; j <- 0 until numCols) {
      C(i, j) = subs(i)(j).evaluate(A(i, j), B(i, j))
    }

    C
  }

}

class SCSignedSaturatingSubtractor() extends Operator {

  private var pAdder = new SCAdder()
  private var nAdder = new SCAdder()
  private var pSub = new SCSaturatingSubtractor()
  private var nSub = new SCSaturatingSubtractor()

  override def evaluate(inputs: List[Tuple2[Int, Int]]): (Int, Int) = {
    val a = inputs(0)
    val b = inputs(1)

    val pp = pAdder.evaluate(a._1, b._1)
    val nn = nAdder.evaluate(a._2, b._2)
    val c1 = pSub.evaluate(pp, nn)
    val c2 = nSub.evaluate(nn, pp)

    (c1, c2)
  }

}

class SCMultiplier() {

  def evaluate(a: Int, b: Int): Int = a & b

}

class SCMatrixMultiplier(numRows: Int, numCols: Int) {

  private var counters = Matrix.zeros[Int](numRows, numCols)

  def evaluate(A: Matrix[Int], B: Matrix[Int]): Matrix[Int] = {
    var C = Matrix.zeros[Int](A.rows, B.cols)
    for (i <- 0 until A.rows; j <- 0 until B.cols) {
      for (k <- 0 until A.cols) {
        counters(i, j) += (A(i, k) & B(k, j))
      }
      if (counters(i, j) >= 1) {
        C(i, j) = 1
        counters(i, j) -= 1
      }
    }

    C
  }

}

class SCSignedMatrixMultiplier(numRows: Int, numCols: Int) extends MatrixOperator {

  private var ppMult = new SCMatrixMultiplier(numRows, numCols)
  private var pnMult = new SCMatrixMultiplier(numRows, numCols)
  private var npMult = new SCMatrixMultiplier(numRows, numCols)
  private var nnMult = new SCMatrixMultiplier(numRows, numCols)
  private var sub11 = new SCMatrixSaturatingSubtractor(numRows, numCols)
  private var sub12 = new SCMatrixSaturatingSubtractor(numRows, numCols)
  private var sub13 = new SCMatrixSaturatingSubtractor(numRows, numCols)
  private var sub14 = new SCMatrixSaturatingSubtractor(numRows, numCols)
  private var sub21 = new SCMatrixSaturatingSubtractor(numRows, numCols)
  private var sub22 = new SCMatrixSaturatingSubtractor(numRows, numCols)
  private var sub23 = new SCMatrixSaturatingSubtractor(numRows, numCols)
  private var sub24 = new SCMatrixSaturatingSubtractor(numRows, numCols)
  private var pAdder = new SCMatrixAdder(numRows, numCols)
  private var nAdder = new SCMatrixAdder(numRows, numCols)

  override def evaluate(inputs: List[Tuple2[Matrix[Int], Matrix[Int]]]):
      (Matrix[Int], Matrix[Int]) = {
    val A = inputs(0)
    val B = inputs(1)

    val pp = ppMult.evaluate(A._1, B._1)
    val pn = pnMult.evaluate(A._1, B._2)
    val np = npMult.evaluate(A._2, B._1)
    val nn = nnMult.evaluate(A._2, B._2)

    val s11 = sub11.evaluate(pp, pn)
    val s12 = sub12.evaluate(pn, pp)
    val s13 = sub13.evaluate(np, nn)
    val s14 = sub14.evaluate(nn, np)

    val s21 = sub21.evaluate(s11, s14)
    val s22 = sub22.evaluate(s12, s13)
    val s23 = sub23.evaluate(s13, s12)
    val s24 = sub24.evaluate(s14, s11)

    val c1 = pAdder.evaluate(s21, s23)
    val c2 = nAdder.evaluate(s22, s24)

    (c1, c2)
  }

}

class SCSignedMultiplier() extends Operator {

  private var ppMult = new SCMultiplier()
  private var pnMult = new SCMultiplier()
  private var npMult = new SCMultiplier()
  private var nnMult = new SCMultiplier()
  private var sub11 = new SCSaturatingSubtractor()
  private var sub12 = new SCSaturatingSubtractor()
  private var sub13 = new SCSaturatingSubtractor()
  private var sub14 = new SCSaturatingSubtractor()
  private var sub21 = new SCSaturatingSubtractor()
  private var sub22 = new SCSaturatingSubtractor()
  private var sub23 = new SCSaturatingSubtractor()
  private var sub24 = new SCSaturatingSubtractor()
  private var pAdder = new SCAdder()
  private var nAdder = new SCAdder()

  override def evaluate(inputs: List[Tuple2[Int, Int]]): (Int, Int) = {
    val a = inputs(0)
    val b = inputs(1)

    val pp = ppMult.evaluate(a._1, b._1)
    val pn = pnMult.evaluate(a._1, b._2)
    val np = npMult.evaluate(a._2, b._1)
    val nn = nnMult.evaluate(a._2, b._2)

    val s11 = sub11.evaluate(pp, pn)
    val s12 = sub12.evaluate(pn, pp)
    val s13 = sub13.evaluate(np, nn)
    val s14 = sub14.evaluate(nn, np)

    val s21 = sub21.evaluate(s11, s14)
    val s22 = sub22.evaluate(s12, s13)
    val s23 = sub23.evaluate(s13, s12)
    val s24 = sub24.evaluate(s14, s11)

    val c1 = pAdder.evaluate(s21, s23)
    val c2 = nAdder.evaluate(s22, s24)

    (c1, c2)
  }

}

class SCDivider() {

  private var counter = 0
  private var bAnd = 0
  private var rng = Random

  def evaluate(a: Int, b: Int): Int = {
    // Update counter
    counter = math.max(counter + 2 * a - 2 * bAnd, -100)

    // Decide output
    val r = math.round(rng.nextInt(65))
    val c = if (counter > r) 1 else 0

    // Update bAnd
    bAnd = c & b

    c
  }

}

class SCSignedDivider() extends Operator {

  private var ppDiv = new SCDivider()
  private var pnDiv = new SCDivider()
  private var npDiv = new SCDivider()
  private var nnDiv = new SCDivider()
  private var sub11 = new SCSaturatingSubtractor()
  private var sub12 = new SCSaturatingSubtractor()
  private var sub13 = new SCSaturatingSubtractor()
  private var sub14 = new SCSaturatingSubtractor()
  private var sub21 = new SCSaturatingSubtractor()
  private var sub22 = new SCSaturatingSubtractor()
  private var sub23 = new SCSaturatingSubtractor()
  private var sub24 = new SCSaturatingSubtractor()
  private var pAdder = new SCAdder()
  private var nAdder = new SCAdder()

  override def evaluate(inputs: List[Tuple2[Int, Int]]): (Int, Int) = {
    val a = inputs(0)
    val b = inputs(1)

    val pp = ppDiv.evaluate(a._1, b._1)
    val pn = pnDiv.evaluate(a._1, b._2)
    val np = npDiv.evaluate(a._2, b._1)
    val nn = nnDiv.evaluate(a._2, b._2)

    val s11 = sub11.evaluate(pp, pn)
    val s12 = sub12.evaluate(pn, pp)
    val s13 = sub13.evaluate(np, nn)
    val s14 = sub14.evaluate(nn, np)

    val s21 = sub21.evaluate(s11, s14)
    val s22 = sub22.evaluate(s12, s13)
    val s23 = sub23.evaluate(s13, s12)
    val s24 = sub24.evaluate(s14, s11)

    val c1 = pAdder.evaluate(s21, s23)
    val c2 = nAdder.evaluate(s22, s24)

    (c1, c2)
  }

}

class SCFixedGainDivider(gain: Double) {

  private var counter = 0

  def evaluate(a: Int): Int = {
    // Update counter
    counter += 255 * a

    // Decide output
    val b = if (counter >= math.round(255 * gain)) 1 else 0

    // Decrement counter
    counter -= b * math.round(255 * gain).toInt

    b
  }

}

class SCSignedFixedGainDivider(gain: Double) extends Operator {

  private var pDiv = new SCFixedGainDivider(gain)
  private var nDiv = new SCFixedGainDivider(gain)

  override def evaluate(inputs: List[Tuple2[Int, Int]]): (Int, Int) = {
    val a = inputs(0)

    val b1 = pDiv.evaluate(a._1)
    val b2 = nDiv.evaluate(a._2)

    (b1, b2)
  }

}

class SCSquareRoot() extends Operator {

  private var counter = 0
  private var bAnd = 0
  private var decorr = Decorrelator(1, 1)

  override def evaluate(inputs: List[Tuple2[Int, Int]]): (Int, Int) = {
    val a = inputs(0)

    // Update counter
    counter = math.max(counter + 4 * a._1 - 4 * bAnd, -100)

    // Decide output
    val r = math.round(512 * math.random())
    val b = if (counter >= r) 1 else 0

    // Update bAnd
    val bDecorr = decorr.evaluate((b, 0))
    bAnd = b & bDecorr._1

    (b, 0)
  }

}

class SCL2Norm(length: Int) extends MatrixOperator {

  private var dot = new SCSignedMatrixMultiplier(1, 1)
  private var root = new SCSquareRoot()

  override def evaluate(inputs: List[Tuple2[Matrix[Int], Matrix[Int]]]):
      (Matrix[Int], Matrix[Int]) = {
    val A = inputs(0)
    val At = (A._1.T, A._2.T)

    val B = dot.evaluate(List(At, A))
    val c = root.evaluate(List((B._1(0, 0), B._2(0, 0))))

    (Matrix(Array(Array(c._1))), Matrix(Array(Array(c._2))))
  }

}