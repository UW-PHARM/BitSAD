package bitstream.types

import math._
import math.Numeric.Implicits._
import math.Numeric._
import Array._
import reflect._
import scala.reflect.runtime.universe._
import scala.util.Random
import bitstream.simulator.Operators._
import scala.io.Source
import java.io.{File, FileWriter, BufferedWriter}

case class Bit(private var _value: Int) {

  def value: Int = _value
  def value_=(value: Int) = {
    _value = value
  }

  def ==(that: Int): Boolean = if (this.value == that) true else false
  def ==(that: Long): Boolean = if (this.value == that) true else false
  def ==(that: Double): Boolean = if (this.value == that) true else false
  def !=(that: Int): Boolean = !(this == that)
  def !=(that: Long): Boolean = !(this == that)
  def !=(that: Double): Boolean = !(this == that)
  def <(that: Int): Boolean = if (this.value < that) true else false
  def <(that: Long): Boolean = if (this.value < that) true else false
  def <(that: Double): Boolean = if (this.value < that) true else false
  def <=(that: Int): Boolean = if (this.value <= that) true else false
  def <=(that: Long): Boolean = if (this.value <= that) true else false
  def <=(that: Double): Boolean = if (this.value <= that) true else false
  def >(that: Int): Boolean = if (this.value > that) true else false
  def >(that: Long): Boolean = if (this.value > that) true else false
  def >(that: Double): Boolean = if (this.value > that) true else false
  def >=(that: Int): Boolean = if (this.value >= that) true else false
  def >=(that: Long): Boolean = if (this.value >= that) true else false
  def >=(that: Double): Boolean = if (this.value >= that) true else false

  def +(that: Int): Int = _value + that
  def +(that: Long): Long = _value + that
  def +(that: Double): Double = _value + that
  def -(that: Int): Int = _value - that
  def -(that: Long): Long = _value - that
  def -(that: Double): Double = _value - that
  def *(that: Int): Int = _value * that
  def *(that: Long): Long = _value * that
  def *(that: Double): Double = _value * that
  def /(that: Int): Int = _value / that
  def /(that: Long): Long = _value / that
  def /(that: Double): Double = _value / that

}

object Bit {

  implicit def bit2Int(x: Bit): Int = x.value
  implicit def int2Bit(x: Int): Bit = Bit(x)

  implicit object BitIsIntegral extends Integral[Bit] {
    // Numeric[SBitstream] abstract methods
    def compare(x: Bit, y: Bit): Int = x.value.compare(y.value)
    def quot(x: Bit, y: Bit): Bit = Bit(x.value / y.value)
    def rem(x: Bit, y: Bit): Bit = Bit(x.value % y.value)
    def fromInt(x: Int): Bit = Bit(x)
    def minus(x: Bit, y: Bit): Bit = Bit(x.value - y.value)
    def negate(x: Bit): Bit = Bit(-x.value)
    def plus(x: Bit, y: Bit): Bit = Bit(x.value + y.value)
    def times(x: Bit, y: Bit): Bit = Bit(x.value * y.value)
    def toDouble(x: Bit): Double = x.value
    def toFloat(x: Bit): Float = x.value.toFloat
    def toInt(x: Bit): Int = x.value.toInt
    def toLong(x: Bit): Long = x.value.toLong
  }

}

trait Bitstream {

  // type Bit

  // def pop: Bit
  // def push(x: Bit): Unit

  def mkString: String

}

case class SBitstream(private var _value: Double,
                      val windowLen: Int = 1000, val numPops: Int = 1,
                      val signedness: SBitstream.Signedness.Signedness =
                        SBitstream.Signedness.Unipolar)
  extends Bitstream {

  // type Bit = Tuple2[Int, Int]

  // private val _id = SBitstream.idGenerator.nextInt()
  // private var rng = Random

  // private val arrLen = math.ceil(windowLen / 32).toInt
  // private var buffer = new Array[Tuple2[Int, Int]](arrLen)
  // private var produceIndex = 0
  // private var consumeIndex = 0

  // for (i <- 0 until arrLen) buffer(i) = (0, 0)

  _value = if (_value > 1) 1.0 else _value
  _value = if (_value < -1) -1.0 else _value

  private var positiveValue: Double = 0.0 // dummy value
  private var negativeValue: Double = 0.0 // dummy value

  signedness match {
    case SBitstream.Signedness.Unipolar => {
      positiveValue = if (_value >= 0) _value else 0.0
      negativeValue = if (_value < 0) _value else 0.0
    }
    case SBitstream.Signedness.Bipolar =>
      throw new NotImplementedError("No support for bipolar signedness of bitstreams")
    case SBitstream.Signedness.SignedMagnitude =>
      throw new NotImplementedError("No support for signed magnitude signedness of bitstreams")
    case _ => throw new IllegalArgumentException("Unrecognized signedness of SBitstream")
  }

  // def id: Int = _id

  def value: Double = _value

  def value_=(value: Double) = {
    require(value >= -1 && value <= 1, "Bitstream value must be in [-1, 1]")
    _value = value
    signedness match {
      case SBitstream.Signedness.Unipolar => {
        if (value >= 0) {
          positiveValue = value
          negativeValue = 0.0
        } else {
          positiveValue = 0.0
          negativeValue = value
        }
      }
      case SBitstream.Signedness.Bipolar =>
      throw new NotImplementedError("No support for bipolar signedness of bitstreams")
      case SBitstream.Signedness.SignedMagnitude =>
        throw new NotImplementedError("No support for signed magnitude signedness of bitstreams")
      case _ => throw new IllegalArgumentException("Unrecognized signedness of SBitstream")
    }
  }

  // private def genBit: (Int, Int) = {
  //   var pBit = 0
  //   var nBit = 0

  //   signedness match {
  //     case SBitstream.Signedness.Unipolar => {
  //       pBit = if (rng.nextDouble() <= positiveValue) 1 else 0
  //       nBit = if (rng.nextDouble() <= negativeValue) 1 else 0
  //     }
  //     case SBitstream.Signedness.Bipolar =>
  //     throw new NotImplementedError("No support for bipolar signedness of bitstreams")
  //     case SBitstream.Signedness.SignedMagnitude =>
  //       throw new NotImplementedError("No support for signed magnitude signedness of bitstreams")
  //     case _ => throw new IllegalArgumentException("Unrecognized signedness of SBitstream")
  //   }

  //   (pBit, nBit)
  // }

  // override def pop: (Int, Int) = {
  //   if (produceIndex == consumeIndex) this.genBit
  //   else {
  //     val idx = consumeIndex % (32 * arrLen)
  //     consumeIndex += 1

  //     val slice = buffer(idx / 32)
  //     val p = (slice._1 >> (idx % 32)) & 1
  //     val n = (slice._2 >> (idx % 32)) & 1
  //     (p, n)
  //   }
  // }
  // override def push(x: Tuple2[Int, Int]): Unit = {
  //   val idx = produceIndex % (32 * arrLen)
  //   produceIndex += 1

  //   var slice = buffer(idx / 32)
  //   buffer(idx / 32) = (slice._1 | (x._1 << (idx % 32)), slice._2 | (x._2 << (idx % 32)))
  // }

  // def getEstimate: Double = {
  //   println("Current buffer:")
  //   println(buffer.mkString)
  //   val accum = (x: Tuple2[Int, Int], y: Tuple2[Int, Int]) => {
  //     var pAccum = 0
  //     var nAccum = 0

  //     for (i <- 0 until 32) {
  //       pAccum += (((x._1 >> i) & 1) + ((y._1 >> i) & 1))
  //       nAccum += (((x._2 >> i) & 1) + ((y._2 >> i) & 1))
  //     }

  //     (pAccum, nAccum)
  //   }
  //   val est = buffer.foldLeft((0, 0))(accum)
  //   // println(s"Total: $est")

  //   (est._1 - est._2).toDouble / windowLen
  // }

  def ==(that: Int): Boolean = if (this.value == that) true else false
  def ==(that: Long): Boolean = if (this.value == that) true else false
  def ==(that: Double): Boolean = if (this.value == that) true else false
  def !=(that: Int): Boolean = !(this == that)
  def !=(that: Long): Boolean = !(this == that)
  def !=(that: Double): Boolean = !(this == that)
  def <(that: Int): Boolean = if (this.value < that) true else false
  def <(that: Long): Boolean = if (this.value < that) true else false
  def <(that: Double): Boolean = if (this.value < that) true else false
  def <=(that: Int): Boolean = if (this.value <= that) true else false
  def <=(that: Long): Boolean = if (this.value <= that) true else false
  def <=(that: Double): Boolean = if (this.value <= that) true else false
  def >(that: Int): Boolean = if (this.value > that) true else false
  def >(that: Long): Boolean = if (this.value > that) true else false
  def >(that: Double): Boolean = if (this.value > that) true else false
  def >=(that: Int): Boolean = if (this.value >= that) true else false
  def >=(that: Long): Boolean = if (this.value >= that) true else false
  def >=(that: Double): Boolean = if (this.value >= that) true else false

  def +(that: SBitstream): SBitstream = SBitstream.SBitstreamIsFractional.plus(this, that)
  def -(that: SBitstream): SBitstream = SBitstream.SBitstreamIsFractional.minus(this, that)
  def *(that: SBitstream): SBitstream = SBitstream.SBitstreamIsFractional.times(this, that)
  def /(that: SBitstream): SBitstream = SBitstream.SBitstreamIsFractional.div(this, that)

  def +(that: Int): SBitstream = this + SBitstream(that.toDouble)
  def +(that: Long): SBitstream = this + SBitstream(that.toDouble)
  def +(that: Double): SBitstream = this + SBitstream(that.toDouble)
  def -(that: Int): SBitstream = this - SBitstream(that.toDouble)
  def -(that: Long): SBitstream = this - SBitstream(that.toDouble)
  def -(that: Double): SBitstream = this - SBitstream(that.toDouble)
  def *(that: Int): SBitstream = this * SBitstream(that.toDouble)
  def *(that: Long): SBitstream = this * SBitstream(that.toDouble)
  def *(that: Double): SBitstream = {
    var x = SBitstream(that.toDouble)
    println(s"${x.value}")
    this * x
  }

  def :/(that: Int): SBitstream = SBitstream(this.value / that)
  def :/(that: Long): SBitstream = SBitstream(this.value / that)
  def :/(that: Double): SBitstream = SBitstream(this.value / that)

  override def mkString: String = s"Bitstream[${_value}, $windowLen ticks, $numPops pops]"

}

object SBitstream {

  // private var idGenerator = Random
  // private var opMap: Map[Tuple3[Int, Int, String], Tuple2[Operator, SBitstream]] = Map()

  def sqrt(stream: SBitstream): SBitstream = SBitstream(math.sqrt(stream.value))

  def abs(stream: SBitstream): SBitstream = SBitstream.SBitstreamIsFractional.abs(stream)

  implicit object Signedness extends Enumeration {
    type Signedness = Value
    val Unipolar, Bipolar, SignedMagnitude = Value
  }

  implicit object SBitstreamIsFractional extends Fractional[SBitstream] {
    // Numeric[SBitstream] abstract methods
    def compare(x: SBitstream, y: SBitstream): Int = x.value.compare(y.value)
    def div(x: SBitstream, y: SBitstream): SBitstream = SBitstream(x.value / y.value)
    def fromInt(x: Int): SBitstream = SBitstream(x)
    def minus(x: SBitstream, y: SBitstream): SBitstream = SBitstream(x.value - y.value)
    def negate(x: SBitstream): SBitstream = SBitstream(-x.value)
    def plus(x: SBitstream, y: SBitstream): SBitstream = SBitstream(x.value + y.value)
    def times(x: SBitstream, y: SBitstream): SBitstream = SBitstream(x.value * y.value)
    def toDouble(x: SBitstream): Double = x.value
    def toFloat(x: SBitstream): Float = x.value.toFloat
    def toInt(x: SBitstream): Int = x.value.toInt
    def toLong(x: SBitstream): Long = x.value.toLong
  }

  implicit class SBitstreamOps[A](lhs: A)(implicit num: Numeric[A]) {
    def +(rhs: SBitstream): SBitstream =
      SBitstream.SBitstreamIsFractional.plus(SBitstream(lhs.toDouble), rhs)
    def -(rhs: SBitstream): SBitstream =
      SBitstream.SBitstreamIsFractional.minus(SBitstream(lhs.toDouble), rhs)
    def *(rhs: SBitstream): SBitstream =
      SBitstream.SBitstreamIsFractional.times(SBitstream(lhs.toDouble), rhs)
    def /(rhs: SBitstream): SBitstream =
      SBitstream.SBitstreamIsFractional.div(SBitstream(lhs.toDouble), rhs)
  }

}

case class DBitstream(val buffLen: Int = 1024) extends Bitstream {

  // type Bit = Int

  private var buffer = new Array[Bit](buffLen)
  private var produceIndex = 0
  private var consumeIndex = 0

  def apply(t: Int): Int = buffer(consumeIndex % buffLen)

  def pop: Bit = {
    if (produceIndex == consumeIndex) 0
    else {
      consumeIndex += 1
      buffer((consumeIndex - 1) % buffLen)
    }
  }
  def push(x: Int): Unit = {
    buffer(produceIndex % buffLen) = x
    produceIndex += 1
  }

  def ==(that: Int): Boolean = buffer(consumeIndex % buffLen) == that
  def ==(that: Long): Boolean = buffer(consumeIndex % buffLen) == that
  def ==(that: Double): Boolean = buffer(consumeIndex % buffLen) == that
  def !=(that: Int): Boolean = buffer(consumeIndex % buffLen) != that
  def !=(that: Long): Boolean = buffer(consumeIndex % buffLen) != that
  def !=(that: Double): Boolean = buffer(consumeIndex % buffLen) != that
  def <(that: Int): Boolean = buffer(consumeIndex % buffLen) < that
  def <(that: Long): Boolean = buffer(consumeIndex % buffLen) < that
  def <(that: Double): Boolean = buffer(consumeIndex % buffLen) < that
  def <=(that: Int): Boolean = buffer(consumeIndex % buffLen) <= that
  def <=(that: Long): Boolean = buffer(consumeIndex % buffLen) <= that
  def <=(that: Double): Boolean = buffer(consumeIndex % buffLen) <= that
  def >(that: Int): Boolean = buffer(consumeIndex % buffLen) > that
  def >(that: Long): Boolean = buffer(consumeIndex % buffLen) > that
  def >(that: Double): Boolean = buffer(consumeIndex % buffLen) > that
  def >=(that: Int): Boolean = buffer(consumeIndex % buffLen) >= that
  def >=(that: Long): Boolean = buffer(consumeIndex % buffLen) >= that
  def >=(that: Double): Boolean = buffer(consumeIndex % buffLen) >= that

  def +(that: DBitstream): Int = this.pop + that.pop
  def -(that: DBitstream): Int = this.pop - that.pop
  def *(that: DBitstream): Int = this.pop * that.pop

  def +(that: Int): Int = this.pop + that
  def +(that: Long): Long = this.pop + that
  def +(that: Double): Double = this.pop + that
  def -(that: Int): Int = this.pop - that
  def -(that: Long): Long = this.pop - that
  def -(that: Double): Double = this.pop - that
  def *(that: Int): Int = this.pop * that
  def *(that: Long): Long = this.pop * that
  def *(that: Double): Double = this.pop * that

  override def mkString: String = buffer(consumeIndex % buffLen).toString

}

object DBitstream {

  def preloadBitstream(filename: String): (DBitstream, Int, Int, Int) = {
    implicit class Regex(sc: StringContext) {
      def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
    }

    var stream = DBitstream()
    var sampleRate = 0
    var numChannels = 0
    var length = 0

    for (line <- Source.fromFile(filename).getLines) line match {
      case r"(?:\s+[\de\-\.]+\s+([\d\.\-]+)${bit}\s+)" => {
        stream.push(math.round(bit.toFloat))
        length += 1
      }
      case r"(?:; Sample Rate (\d+)${rate})" => sampleRate = rate.toInt
      case r"(?:; Channels (\d+)${channels})" => numChannels = channels.toInt
      case default => println(default)
    }

    (stream, sampleRate, numChannels, length)
  }

  def exportBitstream(filename: String, stream: DBitstream,
                      sampleRate: Int, numChannels: Int, length: Int): Unit = {
    val file = new File(filename)
    val writer = new BufferedWriter(new FileWriter(file))
    val stepSize = 1 / sampleRate.toDouble

    writer.write(s"; Sample Rate $sampleRate\n; Channels $numChannels\n")
    for (t <- 0 until length) writer.write(s"  ${t * stepSize}    ${stream.pop} \n")

    writer.close()
  }

  implicit class DBitstreamOps[A](lhs: A)(implicit num: Numeric[A]) {
    def +(rhs: DBitstream): A = num.plus(lhs, num.fromInt(rhs.pop))
    def -(rhs: DBitstream): A = num.minus(lhs, num.fromInt(rhs.pop))
    def *(rhs: DBitstream): A = num.times(lhs, num.fromInt(rhs.pop))
  }

}