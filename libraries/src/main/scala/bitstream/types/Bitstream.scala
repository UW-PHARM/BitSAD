package bitstream.types

import math._
import math.Numeric.Implicits._
import math.Numeric._
import Array._
import reflect._
import scala.reflect.runtime.universe._
import scala.util.Random
import bitstream.simulator.internal.units._
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
                      val signedness: SBitstream.Signedness.Signedness =
                        SBitstream.Signedness.Unipolar)
  extends Bitstream {

  private var rng = new Random()
  var _bit: Tuple2[Int, Int] = (-1, -1)

  _value = if (_value > 1) 1.0 else _value
  _value = if (_value < -1) -1.0 else _value

  var positiveValue: Double = 0.0 // dummy value
  var negativeValue: Double = 0.0 // dummy value

  signedness match {
    case SBitstream.Signedness.Unipolar => {
      positiveValue = if (_value >= 0) _value else 0.0
      negativeValue = if (_value < 0) math.abs(_value) else 0.0
    }
    case SBitstream.Signedness.Bipolar =>
      throw new NotImplementedError("No support for bipolar signedness of bitstreams")
    case SBitstream.Signedness.SignedMagnitude =>
      throw new NotImplementedError("No support for signed magnitude signedness of bitstreams")
    case _ => throw new IllegalArgumentException("Unrecognized signedness of SBitstream")
  }

  this.genBit

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
          negativeValue = math.abs(value)
        }
      }
      case SBitstream.Signedness.Bipolar =>
      throw new NotImplementedError("No support for bipolar signedness of bitstreams")
      case SBitstream.Signedness.SignedMagnitude =>
        throw new NotImplementedError("No support for signed magnitude signedness of bitstreams")
      case _ => throw new IllegalArgumentException("Unrecognized signedness of SBitstream")
    }
  }

  def genBit: Unit = {
    var pBit = 0
    var nBit = 0

    signedness match {
      case SBitstream.Signedness.Unipolar => {
        pBit = if (rng.nextDouble() < positiveValue) 1 else 0
        nBit = if (rng.nextDouble() < negativeValue) 1 else 0
      }
      case SBitstream.Signedness.Bipolar =>
      throw new NotImplementedError("No support for bipolar signedness of bitstreams")
      case SBitstream.Signedness.SignedMagnitude =>
        throw new NotImplementedError("No support for signed magnitude signedness of bitstreams")
      case _ => throw new IllegalArgumentException("Unrecognized signedness of SBitstream")
    }

    _bit = (pBit, nBit)
  }

  def pop: Tuple2[Int, Int] = {
    // if (_bit == (-1, -1)) this.genBit
    // else {
    //   val out = _bit
    //   _bit = (-1, -1)
    //   out
    // }
    _bit
  }

  def push(x: Tuple2[Int, Int]): Unit = {
    _bit = x
  }

  def observe: Tuple2[Int, Int] = {
    // if (_bit == (-1, -1)) {
    //   _bit = this.genBit
    //   _bit
    // } else _bit
    _bit
  }

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

  def +(that: SBitstream)(implicit id: SimulationId): SBitstream
    = SimulatableNumeric.SBitstreamIsSimulatable.plus(this, that)(id.lhs, id.rhs)
  def -(that: SBitstream)(implicit id: SimulationId): SBitstream
    = SimulatableNumeric.SBitstreamIsSimulatable.minus(this, that)(id.lhs, id.rhs)
  def *(that: SBitstream)(implicit id: SimulationId): SBitstream
    = SimulatableNumeric.SBitstreamIsSimulatable.times(this, that)(id.lhs, id.rhs)
  def /(that: SBitstream)(implicit id: SimulationId): SBitstream
    = SimulatableNumeric.SBitstreamIsSimulatable.div(this, that)(id.lhs, id.rhs)

  def +(that: Int)(implicit id: SimulationId): SBitstream
    = (this + SBitstream(that.toDouble))(id)
  def +(that: Long)(implicit id: SimulationId): SBitstream
    = (this + SBitstream(that.toDouble))(id)
  def +(that: Double)(implicit id: SimulationId): SBitstream
    = (this + SBitstream(that.toDouble))(id)
  def -(that: Int)(implicit id: SimulationId): SBitstream
    = (this - SBitstream(that.toDouble))(id)
  def -(that: Long)(implicit id: SimulationId): SBitstream
    = (this - SBitstream(that.toDouble))(id)
  def -(that: Double)(implicit id: SimulationId): SBitstream
    = (this - SBitstream(that.toDouble))(id)
  def *(that: Int)(implicit id: SimulationId): SBitstream
    = (this * SBitstream(that.toDouble))(id)
  def *(that: Long)(implicit id: SimulationId): SBitstream
    = (this * SBitstream(that.toDouble))(id)
  def *(that: Double)(implicit id: SimulationId): SBitstream
    = (this * SBitstream(that.toDouble))(id)

  def :/(that: Int)(implicit id: SimulationId): SBitstream = {
    var out = SBitstream(this.value / that)
    var op = SBitstream.findOperator(id.lhs, id.rhs, "fdiv")
    out.push(op.evaluate(List(this.pop)))
    out
  }
  def :/(that: Long)(implicit id: SimulationId): SBitstream = {
    var out = SBitstream(this.value / that.toInt)
    var op = SBitstream.findOperator(id.lhs, id.rhs, "fdiv")
    out.push(op.evaluate(List(this.pop)))
    out
  }
  def :/(that: Double)(implicit id: SimulationId): SBitstream = {
    var out = SBitstream(this.value / math.round(that))
    var op = SBitstream.findOperator(id.lhs, id.rhs, "fdiv")
    out.push(op.evaluate(List(this.pop)))
    out
  }

  override def mkString: String = s"Bitstream[${_value}]"

}

object SBitstream {

  private var opMap: Map[Tuple3[String, String, String], Operator] = Map()

  def findOperator(xId: String, yId: String, op: String): Operator = {
    if (opMap contains (xId, yId, op)) opMap((xId, yId, op))
    else op match {
      case "plus" => {
        var adder = new SCSignedAdder()
        opMap += ((xId, yId, "plus") -> adder)
        opMap((xId, yId, op))
      }
      case "minus" => {
        var subtractor = new SCSignedSaturatingSubtractor()
        opMap += ((xId, yId, "minus") -> subtractor)
        opMap((xId, yId, op))
      }
      case "times" => {
        var multiplier = new SCSignedMultiplier()
        opMap += ((xId, yId, "times") -> multiplier)
        opMap((xId, yId, op))
      }
      case "div" => {
        var divider = new SCSignedDivider()
        opMap += ((xId, yId, "div") -> divider)
        opMap((xId, yId, op))
      }
      case "fdiv" => {
        var divider = new SCSignedFixedGainDivider(math.ceil(yId.toDouble).toInt)
        opMap += ((xId, yId, "fdiv") -> divider)
        opMap((xId, yId, op))
      }
      case "sqrt" => {
        var rooter = new SCSquareRoot()
        opMap += ((xId, yId, "sqrt") -> rooter)
        opMap((xId, yId, op))
      }
    }
  }

  def sqrt(stream: SBitstream)(implicit id: SimulationId): SBitstream
    = SimulatableNumeric.SBitstreamIsSimulatable.sqrt(stream)(id.lhs, id.rhs)

  def abs(stream: SBitstream): SBitstream = SBitstream.SBitstreamIsFractional.abs(stream)

  implicit object Signedness extends Enumeration {
    type Signedness = Value
    val Unipolar, Bipolar, SignedMagnitude = Value
  }

  trait SBitstreamIsFractional extends Fractional[SBitstream] {
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
  implicit object SBitstreamIsFractional extends SBitstreamIsFractional

  implicit class SBitstreamOps[A](lhs: A)(implicit num: Numeric[A]) {
    def +(rhs: SBitstream)(implicit id: SimulationId): SBitstream =
      SimulatableNumeric.SBitstreamIsSimulatable.plus(SBitstream(lhs.toDouble), rhs)(id.lhs, id.rhs)
    def -(rhs: SBitstream)(implicit id: SimulationId): SBitstream =
      SimulatableNumeric.SBitstreamIsSimulatable.minus(SBitstream(lhs.toDouble), rhs)(id.lhs, id.rhs)
    def *(rhs: SBitstream)(implicit id: SimulationId): SBitstream =
      SimulatableNumeric.SBitstreamIsSimulatable.times(SBitstream(lhs.toDouble), rhs)(id.lhs, id.rhs)
    def /(rhs: SBitstream)(implicit id: SimulationId): SBitstream =
      SimulatableNumeric.SBitstreamIsSimulatable.div(SBitstream(lhs.toDouble), rhs)(id.lhs, id.rhs)
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