package bitstream.simulator.units

import reflect._
import Array._
import scala.collection.mutable.ArrayBuffer
import bitstream.types._
import math._
import scala.util.Random

// trait BitstreamSpecificType[A] {

//   type Bit

// }

// object BitstreamSpecificType {

//   implicit object SBitstreamType extends BitstreamSpecificType[SBitstream] {
//     type Bit = Tuple2[Int, Int]
//   }

//   implicit object DBitstreamType extends BitstreamSpecificType[DBitstream] {
//     type Bit = Int
//   }

// }

// Mersenne Twister 19937
// Taken from : https://gist.github.com/jesperdj/887771
// Based on code from: http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
// Note: This implementation is not thread-safe!
final class MersenneTwister (seed: Int = 5489) {
  private val N = 624
  private val M = 397

  private val MatrixA = 0x9908b0dfL

  private val UpperMask = 0x80000000L
  private val LowerMask = 0x7fffffffL

  private val mt = new Array[Long](N)
  private var mti = N + 1

  mt(0) = seed
  for (i <- 1 until N) mt(i) = (1812433253L * (mt(i - 1) ^ (mt(i - 1) >>> 30)) + i) & 0xffffffffL

  // Generates the next random integer in the sequence
  def nextInt(): Int = {
    var y = 0L

    if (mti >= N) {
      val mag01 = Array(0L, MatrixA)

      var kk = 0
      while (kk < N - M) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + M) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)
        kk += 1
      }
      while (kk < N - 1) {
        y = (mt(kk) & UpperMask) | (mt(kk + 1) & LowerMask)
        mt(kk) = mt(kk + (M - N)) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)
        kk += 1
      }
      y = (mt(N - 1) & UpperMask) | (mt(0) & LowerMask)
      mt(N - 1) = mt(M - 1) ^ (y >>> 1) ^ mag01(y.toInt & 0x1)

      mti = 0
    }

    y = mt(mti); mti += 1
    y ^= y >>> 11
    y ^= (y << 7) & 0x9d2c5680L
    y ^= (y << 15) & 0xefc60000L
    y ^= (y >>> 18)

    y.toInt
  }

  // Generates a random integer in the interval [0, limit)
  def nextInt(limit: Int): Int = {
    // Find shift distance
    val lim = limit.toLong & 0xffffffffL
    var n = -1; var bit = 1L << 32
    while (bit > lim) { n += 1; bit >>>= 1 }

    // Generate integer, take most significant bits; reject while outside interval
    var r = (nextInt().toLong & 0xffffffffL) >>> n
    while (r >= lim) { r = (nextInt().toLong & 0xffffffffL) >>> n }
    r.toInt
  }

  // Generates a random Double in the interval [0, 1)
  def nextDouble(): Double = {
    val a: Long = (nextInt().toLong & 0xffffffffL) >>> 5
    val b: Long = (nextInt().toLong & 0xffffffffL) >>> 6
    (a * 67108864.0 + b) / 9007199254740992.0
  }
}

case class Recorder (val windowLen: Int, val rows: Int = 1, val cols: Int = 1) {

  private var buffer = new ArrayBuffer[Tuple2[Matrix[Int], Matrix[Int]]](windowLen)
  private var index = 0

  // initiliaze the buffer
  this.clear()

  def record(x: Tuple2[Int, Int]): Unit = {
    require(rows == 1 && cols == 1,
      s"Recorder has size $rows by $cols but you recorded a single bit")
    buffer(index % windowLen) = (Matrix(Array(Array(x._1))), Matrix(Array(Array(x._2))))
    index += 1
  }

  def record(x: Tuple2[Matrix[Int], Matrix[Int]])(implicit dnc: DummyImplicit): Unit = {
    require(rows == x._1.rows && cols == x._1.cols,
      s"Recorder has size $rows by $cols but you recorded a matrix of size ${x._1.rows} by ${x._1.cols}")
    buffer(index % windowLen) = x
    index += 1
  }

  def clear(): Unit = {
    var empty = (Matrix.zeros[Int](rows, cols), Matrix.zeros[Int](rows, cols))
    buffer = ArrayBuffer.fill(windowLen)(empty)
  }

  def estimate: Matrix[Double] = {
    var result = Matrix.zeros[Double](rows, cols)

    for (t <- 0 until windowLen) {
      var bits = buffer(t)
      for (i <- 0 until rows; j <- 0 until cols) {
        val pBit = bits._1(i, j)
        val nBit = bits._2(i, j)
        result(i, j) = result(i, j) + pBit - nBit
      }
    }

    result / windowLen.toDouble
  }

}

case class Decorrelator(val _numRows: Int, val _numCols: Int,
                        val _stepVal: Int = 16, val _rngRange: Int = 65535) {

  private var rng = new MersenneTwister(Random.nextInt())
  private var countersP = Matrix.zeros[Int](_numRows, _numCols)
  private var countersN = Matrix.zeros[Int](_numRows, _numCols)
  private var bufferP = new Array[Matrix[Int]](2)
  private var bufferN = new Array[Matrix[Int]](2)

  bufferP(0) = Matrix.zeros[Int](_numRows, _numCols)
  bufferP(1) = Matrix.zeros[Int](_numRows, _numCols)
  bufferN(0) = Matrix.zeros[Int](_numRows, _numCols)
  bufferN(1) = Matrix.zeros[Int](_numRows, _numCols)

  def rows: Int = _numRows
  def cols: Int = _numCols

  def evaluate(x: Tuple2[Int, Int])(implicit i: DummyImplicit): (Int, Int) = {
    require(_numRows == 1 && _numCols == 1,
      "Decorrelator of size > (1, 1) expects Matrix input")

    var resultP = Matrix.zeros[Int](_numRows, _numCols)
    var resultN = Matrix.zeros[Int](_numRows, _numCols)

    // Increment counters
    countersP = countersP + _stepVal * x._1
    countersN = countersN + _stepVal * x._2

    // Output spike
    resultP = bufferP(0)
    resultN = bufferN(0)
    bufferP(0) = bufferP(1)
    bufferN(0) = bufferN(1)
    val rP = rng.nextInt(_rngRange + 1)
    val rN = rng.nextInt(_rngRange + 1)
    bufferP(1)(0, 0) = if (rP <= countersP(0, 0)) 1 else 0
    bufferN(1)(0, 0) = if (rN <= countersN(0, 0)) 1 else 0

    // Decrement counters
    countersP = countersP - _stepVal * resultP
    countersN = countersN - _stepVal * resultN

    (resultP(0, 0), resultN(0, 0))
  }

  def evaluate(x: Tuple2[Matrix[Int], Matrix[Int]]): (Matrix[Int], Matrix[Int]) = {
    var resultP = Matrix.zeros[Int](_numRows, _numCols)
    var resultN = Matrix.zeros[Int](_numRows, _numCols)

    // Increment counters
    countersP = countersP + _stepVal * x._1
    countersN = countersN + _stepVal * x._2

    // Output spike
    resultP = bufferP(0)
    resultN = bufferN(0)
    bufferP(0) = bufferP(1)
    bufferN(0) = bufferN(1)
    for (i <- 0 until _numRows; j <- 0 until _numCols) {
      val rP = rng.nextInt(_rngRange + 1)
      val rN = rng.nextInt(_rngRange + 1)
      bufferP(1)(i, j) = if (rP <= countersP(i, j)) 1 else 0
      bufferN(1)(i, j) = if (rN <= countersN(i, j)) 1 else 0
    }

    // Decrement counters
    countersP = countersP - _stepVal * resultP
    countersN = countersN - _stepVal * resultN

    (resultP, resultN)
  }

}

case class DelayBuffer(private val _delay:Int) {

  // private val _delay = _delayDouble.toInt

  private var buffer = new Array[Bit](_delay)
  private var produceIndex = 0
  private var consumeIndex = 0

  // Fill delay buffer with empty values
  for (i <- 0 until _delay) this.push(0)

  def delay: Bit = _delay

  def pop: Bit = {
    if (produceIndex == consumeIndex)
      throw new Exception("Tried to pop from an empty DelayBuffer")
    else {
      consumeIndex += 1
      buffer((consumeIndex - 1) % _delay)
    }
  }
  def push(x: Bit): Unit = {
    buffer(produceIndex % _delay) = x
    produceIndex += 1
  }

  def evaluate(x: Bit): Bit = {
    this.push(x)
    this.pop
  }

}

case class SDM() {

  private var _error: Double = 0.0

  def error: Double = _error

  def evaluate(input: Double): Bit = {
    val output = if (input >= _error) 1 else -1
    _error = output - input + _error
    output
  }

}