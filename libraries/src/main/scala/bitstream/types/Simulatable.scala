package bitstream.types

import math._
import math.Numeric.Implicits._
import math.Numeric._

case class SimulationId(lhs: String, rhs: String)

object SimulationId {
  implicit val DefaultId: SimulationId = SimulationId("", "")
}

trait SimulatableNumeric[A] extends Numeric[A] {
  def div(x: A, y: A)(xName: String, yName: String): A
  def minus(x: A, y: A)(xName: String, yName: String): A
  def plus(x: A, y: A)(xName: String, yName: String): A
  def times(x: A, y: A)(xName: String, yName: String): A

  def sqrt(x: A)(xName: String, yName: String): A
  def sqrtDouble(x: A): Double
  def sqrtSBitstream(x: A): SBitstream
  def rand(): A
}

object SimulatableNumeric {
  implicit object IntIsSimulatable extends SimulatableNumeric[Int]
                                   with IntIsIntegral
                                   with Ordering.IntOrdering {
    def div(x: Int, y: Int)(xName: String, yName: String): Int = x / y
    def minus(x: Int, y: Int)(xName: String, yName: String): Int = x - y
    def plus(x: Int, y: Int)(xName: String, yName: String): Int = x + y
    def times(x: Int, y: Int)(xName: String, yName: String): Int = x * y

    def sqrt(x: Int)(xName: String, yName: String): Int = math.sqrt(x).toInt
    def sqrtDouble(x: Int): Double = math.sqrt(x)
    def sqrtSBitstream(x: Int): SBitstream = SBitstream(math.sqrt(x))
    def rand(): Int = math.random().toInt

    // def fromInt(x: Int): Int = x
    // def toDouble(x: Int): Double = x.toDouble
    // def toFloat(x: Int): Float = x.toFloat
    // def toInt(x: Int): Int = x
    // def toLong(x: Int): Long = x.toLong
  }

  implicit object LongIsSimulatable extends SimulatableNumeric[Long]
                                    with LongIsIntegral
                                    with Ordering.LongOrdering {
    def div(x: Long, y: Long)(xName: String, yName: String): Long = x / y
    def minus(x: Long, y: Long)(xName: String, yName: String): Long = x - y
    def plus(x: Long, y: Long)(xName: String, yName: String): Long = x + y
    def times(x: Long, y: Long)(xName: String, yName: String): Long = x * y

    def sqrt(x: Long)(xName: String, yName: String): Long = math.sqrt(x).toLong
    def sqrtDouble(x: Long): Double = math.sqrt(x)
    def sqrtSBitstream(x: Long): SBitstream = SBitstream(math.sqrt(x))
    def rand(): Long = math.random().toLong

    // def fromInt(x: Int): Long = x.toLong
    // def toDouble(x: Long): Double = x.toDouble
    // def toFloat(x: Long): Float = x.toFloat
    // def toInt(x: Long): Int = x.toInt
    // def toLong(x: Long): Long = x
  }

  implicit object FloatIsSimulatable extends SimulatableNumeric[Float]
                                     with FloatIsFractional
                                     with Ordering.FloatOrdering {
    def div(x: Float, y: Float)(xName: String, yName: String): Float = x / y
    def minus(x: Float, y: Float)(xName: String, yName: String): Float = x - y
    def plus(x: Float, y: Float)(xName: String, yName: String): Float = x + y
    def times(x: Float, y: Float)(xName: String, yName: String): Float = x * y

    def sqrt(x: Float)(xName: String, yName: String): Float = math.sqrt(x).toFloat
    def sqrtDouble(x: Float): Double = math.sqrt(x)
    def sqrtSBitstream(x: Float): SBitstream = SBitstream(math.sqrt(x))
    def rand(): Float = math.random().toFloat

    // def fromInt(x: Int): Float = x.toFloat
    // def toDouble(x: Float): Double = x.toDouble
    // def toFloat(x: Float): Float = x
    // def toInt(x: Float): Int = x.toInt
    // def toLong(x: Float): Long = x.toLong
  }

  implicit object DoubleIsSimulatable extends SimulatableNumeric[Double]
                                      with DoubleIsFractional
                                      with Ordering.DoubleOrdering {
    def div(x: Double, y: Double)(xName: String, yName: String): Double = x / y
    def minus(x: Double, y: Double)(xName: String, yName: String): Double = x - y
    def plus(x: Double, y: Double)(xName: String, yName: String): Double = x + y
    def times(x: Double, y: Double)(xName: String, yName: String): Double = x * y

    def sqrt(x: Double)(xName: String, yName: String): Double = math.sqrt(x)
    def sqrtDouble(x: Double): Double = math.sqrt(x)
    def sqrtSBitstream(x: Double): SBitstream = SBitstream(math.sqrt(x))
    def rand(): Double = math.random()

    // def fromInt(x: Int): Double = x.toDouble
    // def toDouble(x: Double): Double = x
    // def toFloat(x: Double): Float = x.toFloat
    // def toInt(x: Double): Int = x.toInt
    // def toLong(x: Double): Long = x.toLong
  }

  implicit object SBitstreamIsSimulatable extends SimulatableNumeric[SBitstream]
                                          with SBitstream.SBitstreamIsFractional {
    def div(x: SBitstream, y: SBitstream)(xName: String, yName: String): SBitstream
      = (xName, yName) match {
        case ("", "") => SBitstream.SBitstreamIsFractional.div(x, y)
        case _ => {
          var z = SBitstream.SBitstreamIsFractional.div(x, y)
          var op = SBitstream.findOperator(xName, yName, "div")
          z.push(op.evaluate(List(x.pop, y.pop)))
          z
        }
    }
    def minus(x: SBitstream, y: SBitstream)(xName: String, yName: String): SBitstream
      = (xName, yName) match {
        case ("", "") => SBitstream.SBitstreamIsFractional.minus(x, y)
        case _ => {
          var z = SBitstream.SBitstreamIsFractional.minus(x, y)
          var op = SBitstream.findOperator(xName, yName, "minus")
          z.push(op.evaluate(List(x.pop, y.pop)))
          z
        }
    }
    def plus(x: SBitstream, y: SBitstream)(xName: String, yName: String): SBitstream
      = (xName, yName) match {
        case ("", "") => SBitstream.SBitstreamIsFractional.plus(x, y)
        case _ => {
          var z = SBitstream.SBitstreamIsFractional.plus(x, y)
          var op = SBitstream.findOperator(xName, yName, "plus")
          z.push(op.evaluate(List(x.pop, y.pop)))
          z
        }
    }
    def times(x: SBitstream, y: SBitstream)(xName: String, yName: String): SBitstream
      = (xName, yName) match {
        case ("", "") => SBitstream.SBitstreamIsFractional.times(x, y)
        case _ => {
          var z = SBitstream.SBitstreamIsFractional.times(x, y)
          var op = SBitstream.findOperator(xName, yName, "times")
          z.push(op.evaluate(List(x.pop, y.pop)))
          z
        }
    }

    def sqrt(x: SBitstream)(xName: String, yName: String): SBitstream
      = (xName, yName) match {
        case ("", "") => SBitstream(math.sqrt(x.value))
        case _ => {
          var z = SBitstream(math.sqrt(x.value))
          var op = SBitstream.findOperator(xName, yName, "sqrt")
          z.push(op.evaluate(List(x.pop)))
          z
        }
    }
    def sqrtDouble(x: SBitstream): Double = math.sqrt(x.value)
    def sqrtSBitstream(x: SBitstream): SBitstream = SBitstream(math.sqrt(x.value))
    def rand(): SBitstream = SBitstream(math.random())

    // def fromInt(x: Int): SBitstream = SBitstream.SBitstreamIsFractional.fromInt(x)
    // def toDouble(x: SBitstream): Double = x.toDouble
    // def toFloat(x: SBitstream): Float = x.toFloat
    // def toInt(x: SBitstream): Int = x.toInt
    // def toLong(x: SBitstream): Long = x.toLong
  }
}