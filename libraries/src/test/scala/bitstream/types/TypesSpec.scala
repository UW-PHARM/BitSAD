package bitstream.types

import org.scalatest._
import org.scalactic.TolerantNumerics._

class TypesSpec extends FunSuite {
  test("Bitstream --- value test") {
    var stream = Bitstream(0.4)
    assert(stream.value === 0.4)
    stream.value = 0.5
    assert(stream.value === 0.5)
    assertThrows[IllegalArgumentException] {
      stream.value = 5.0
    }
    val saturatedStream = Bitstream(2.0)
    assert(saturatedStream.value === 1.0)
  }

  test("Bitstream --- == and != operator test") {
    var a = Bitstream(0.1)
    var b = Bitstream(-0.2)
    var c = Bitstream(0.1)
    var d = Bitstream(-0.2)
    var e = Bitstream(1.0)

    assert((a == b) === false)
    assert((a == c) === true)
    assert((b == c) === false)
    assert((b == d) === true)

    assert((a != b) === true)
    assert((a != c) === false)
    assert((b != c) === true)
    assert((b != d) === false)

    assert((a == 0.1) === true)
    assert((a != 0.1) === false)
    assert((e == 1) === true)
    assert((e != 1) === false)
    assert((e == 1.toLong) === true)
    assert((e != 1.toLong) === false)
  }

  test("Bitstream --- + operator test") {
    var x = Bitstream(0.2)
    var y = Bitstream(0.3)
    var z = Bitstream(-0.1)

    assert((x + y) === Bitstream(0.5))
    assert((x + z) === Bitstream(0.1))
    assert((z + z) === Bitstream(-0.2))
    assert((x + z + z + z) === Bitstream(-0.1))
  }

  test("Bitstream --- - operator test") {
    implicit val doubleEquality = tolerantDoubleEquality(0.001)

    var x = Bitstream(0.2)
    var y = Bitstream(0.3)
    var z = Bitstream(-0.1)

    // need to compare using .value in order to use tolerant equality tests
    assert((y - x).value === 0.1)
    assert((x - y).value === -0.1)
    assert((y - z).value === 0.4)
    assert((z - z).value === 0.0)
    assert((z - x).value === -0.3)
  }

  test("Bitstream --- * operator test") {
    implicit val doubleEquality = tolerantDoubleEquality(0.001)

    var x = Bitstream(0.2)
    var y = Bitstream(0.3)
    var z = Bitstream(-0.1)

    // need to compare using .value in order to use tolerant equality tests
    assert((x * y).value === 0.06)
    assert((x * z).value === -0.02)
    assert((z * z).value === 0.01)
  }

  test("Bitstream --- / operator test") {
    var x = Bitstream(0.2)
    var y = Bitstream(0.4)
    var z = Bitstream(-0.4)

    assert((x / y) === Bitstream(0.5))
    assert((x / z) === Bitstream(-0.5))
    assert((z / z) === Bitstream(1.0))
    assert((y / x) === Bitstream(1.0))
  }

  test("Bitstream --- sqrt() test") {
    var x = Bitstream(0.5)
    var y = Bitstream(-0.5)

    assert((Bitstream.sqrt(x) == math.sqrt(0.5)) === true)
    assert(Bitstream.sqrt(y).value.isNaN === true)
  }

  test("Bitstream --- abs() test") {
    var x = Bitstream(0.5)
    var y = Bitstream(-0.5)

    assert((Bitstream.abs(x) == 0.5) === true)
    assert((Bitstream.abs(y) == 0.5) === true)
  }

  test("Matrix --- constructor test") {
    var A = Matrix[Int](2, 2)

    assert(A.rows === 2)
    assert(A.cols === 2)
    assert(A.size === (2, 2))

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
    }
    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      assert(A(i, j) === i - j)
    }
  }

  test("Matrix --- == and != operator test") {
    var A = Matrix[Int](2, 2)
    var B = Matrix[Int](2, 2)
    var C = Matrix[Int](2, 2)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
      B(i, j) = i - j
      C(i, j) = j - i
    }
    assert((A == B) === true)
    assert((B == C) === false)
    assert((A != C) === true)
    assert((A != B) === false)
  }

  test("Matrix --- T operator test") {
    var A = Matrix[Int](4, 3)
    var B = Matrix[Int](3, 4)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
      B(j, i) = i - j
    }
    assert(A.T === B)
  }

  test("Matrix --- zeros() test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix.zeros[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = 0
    }

    assert((A == B) === true)
  }

  test("Matrix --- ones() test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix.ones[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = 1
    }

    assert((A == B) === true)
  }

  test("Matrix --- eye() test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix.eye[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      if (i == j) A(i, j) = 1
      else A(i, j) = 0
    }

    assert((A == B) === true)
  }

  test("Matrix --- + operator test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
      B(i, j) = j - i
    }
    assert((A + B) === Matrix.zeros[Int](3, 3))
  }

  test("Matrix --- - operator test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
      B(i, j) = i - j
    }
    assert((A - B) === Matrix.zeros[Int](3, 3))
  }

  test("Matrix --- ** operator test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix[Int](3, 3)
    var C = Matrix[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
      B(i, j) = 2
      C(i, j) = 2 * (i - j)
    }
    assert((A ** B) === C)
  }

  test("Matrix --- / operator test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix[Int](3, 3)
    var C = Matrix[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = 2 * (i - j)
      B(i, j) = 2
      C(i, j) = i - j
    }
    assert((A / B) === C)
  }

  test("Matrix --- + (scalar) operator test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
      B(i, j) = i - j + 2
    }
    assert((A + 2) === B)
  }

  test("Matrix --- - (scalar) operator test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
      B(i, j) = i - j - 2
    }
    assert((A - 2) === Matrix.zeros[Int](3, 3))
  }

  test("Matrix --- * (scalar) operator test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
      B(i, j) = 2 * (i - j)
    }
    assert((A * 2) === B)
  }

  test("Matrix --- / (scalar) operator test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = 2 * (i - j)
      B(i, j) = i - j
    }
    assert((A / 2) === B)
  }

  test("Matrix --- * operator test") {
    var A = Matrix[Int](3, 3)
    var B = Matrix[Int](3, 3)
    var C = Matrix[Int](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = i - j
      B(i, j) = i - j

      for (k <- 0 until A.cols) {
        C(i, j) = (i - k) * (k - j)
      }
    }
    assert((A * B) === C)
  }

  test("Matrix --- dot() test") {
    var a = Matrix.ones[Int](3, 1)
    var b = Matrix.ones[Int](3, 1)

    assert(a.dot(b) === 3)
    assert(a.dot(b.T) === 3)
    assert(a.T.dot(b.T) === 3)
    assert(a.T.dot(b) === 3)

    var c = Matrix.ones[Int](3, 3)
    assertThrows[IllegalArgumentException] {
      c.dot(b)
    }
    assertThrows[IllegalArgumentException] {
      b.dot(c)
    }
  }

  test("Matrix --- sqrt() test") {
    var A = Matrix.ones[Double](3, 1)

    A = Matrix.sqrt(A / 2)
    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      assert(A(i, j) === math.sqrt(0.5))
    }
  }

  test("Matrix --- norm() test") {
    var A = Matrix.ones[Double](3, 3)
    var v = Matrix.ones[Double](3, 1)

    assert(Matrix.norm(v) === math.sqrt(3))
    assert(Matrix.norm(v, "L1") === 3)
    assert(Matrix.norm(v, "inf") === 1)
    assert(Matrix.norm(v, "fro") === math.sqrt(3))

    assertThrows[NotImplementedError] {
      Matrix.norm(A)
    }
    assert(Matrix.norm(A, "L1") === 3)
    assert(Matrix.norm(A, "inf") === 3)
    assert(Matrix.norm(A, "fro") === 3)
  }

  test("Matrix --- rand() test") {
    var A = Matrix.rand[Int](3, 3)
    var B = Matrix.rand[Double](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      assert((A(i, j) == 1.0 || A(i, j) == 0.0) === true)
      assert((A(i, j) <= 1.0 && A(i, j) >= 0.0) === true)
    }
  }

  test("Matrix[Bitstream] --- constructor test") {
    var A = Matrix[Bitstream](2, 2)

    assert(A.rows === 2)
    assert(A.cols === 2)
    assert(A.size === (2, 2))

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
    }
    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      assert(A(i, j).value === i - j)
    }
  }

  test("Matrix[Bitstream] --- == and != operator test") {
    var A = Matrix[Bitstream](2, 2)
    var B = Matrix[Bitstream](2, 2)
    var C = Matrix[Bitstream](2, 2)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
      B(i, j) = Bitstream(i - j)
      C(i, j) = Bitstream(j - i)
    }
    assert((A == B) === true)
    assert((B == C) === false)
    assert((A != C) === true)
    assert((A != B) === false)
  }

  test("Matrix[Bitstream] --- T operator test") {
    var A = Matrix[Bitstream](4, 3)
    var B = Matrix[Bitstream](3, 4)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
      B(j, i) = Bitstream(i - j)
    }
    assert(A.T === B)
  }

  test("Matrix[Bitstream] --- zeros() test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix.zeros[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(0)
    }

    assert((A == B) === true)
  }

  test("Matrix[Bitstream] --- ones() test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix.ones[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(1)
    }

    assert((A == B) === true)
  }

  test("Matrix[Bitstream] --- eye() test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix.eye[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      if (i == j) A(i, j) = Bitstream(1)
      else A(i, j) = Bitstream(0)
    }

    assert((A == B) === true)
  }

  test("Matrix[Bitstream] --- + operator test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
      B(i, j) = Bitstream(j - i)
    }
    assert((A + B) === Matrix.zeros[Bitstream](3, 3))
  }

  test("Matrix[Bitstream] --- - operator test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
      B(i, j) = Bitstream(i - j)
    }
    assert((A - B) === Matrix.zeros[Bitstream](3, 3))
  }

  test("Matrix[Bitstream] --- ** operator test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix[Bitstream](3, 3)
    var C = Matrix[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
      B(i, j) = Bitstream(2)
      C(i, j) = Bitstream(2 * (i - j))
    }
    assert((A ** B) === C)
  }

  test("Matrix[Bitstream] --- / operator test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix[Bitstream](3, 3)
    var C = Matrix[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(2 * (i - j))
      B(i, j) = Bitstream(2)
      C(i, j) = Bitstream(i - j)
    }
    assert((A / B) === C)
  }

  test("Matrix[Bitstream] --- + (scalar) operator test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
      B(i, j) = Bitstream(i - j + 2)
    }
    assert((A + Bitstream(2)) === B)
  }

  test("Matrix[Bitstream] --- - (scalar) operator test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
      B(i, j) = Bitstream(i - j - 2)
    }
    assert((A - Bitstream(2)) === Matrix.zeros[Bitstream](3, 3))
  }

  test("Matrix[Bitstream] --- * (scalar) operator test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
      B(i, j) = Bitstream(2 * (i - j))
    }
    assert((A * Bitstream(2)) === B)
  }

  test("Matrix[Bitstream] --- / (scalar) operator test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(2 * (i - j))
      B(i, j) = Bitstream(i - j)
    }
    assert((A / Bitstream(2)) === B)
  }

  test("Matrix[Bitstream] --- * operator test") {
    var A = Matrix[Bitstream](3, 3)
    var B = Matrix[Bitstream](3, 3)
    var C = Matrix[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      A(i, j) = Bitstream(i - j)
      B(i, j) = Bitstream(i - j)

      for (k <- 0 until A.cols) {
        C(i, j) = Bitstream((i - k) * (k - j))
      }
    }
    assert((A * B) === C)
  }

  test("Matrix[Bitstream] --- dot() test") {
    var a = Matrix.ones[Bitstream](3, 1)
    var b = Matrix.ones[Bitstream](3, 1)

    a = a * Bitstream(1.0/3)
    b = b * Bitstream(1.0/3)

    assert(a.dot(b).value === 1.0/3)
    assert(a.dot(b.T).value === 1.0/3)
    assert(a.T.dot(b.T).value === 1.0/3)
    assert(a.T.dot(b).value === 1.0/3)

    var c = Matrix.ones[Bitstream](3, 3)
    assertThrows[IllegalArgumentException] {
      c.dot(b)
    }
    assertThrows[IllegalArgumentException] {
      b.dot(c)
    }
  }

  test("Matrix[Bitstream] --- sqrt() test") {
    var A = Matrix.ones[Bitstream](3, 1)

    A = Matrix.sqrt(A * Bitstream(0.5))
    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      assert(A(i, j).value === math.sqrt(0.5))
    }
  }

  test("Matrix[Bitstream] --- norm() test") {
    var A = Matrix.ones[Bitstream](3, 3)
    var v = Matrix.ones[Bitstream](3, 1)

    v = v * Bitstream(math.sqrt(1.0/3))

    assert(Matrix.norm(v).value === 1.0)
    assertThrows[NotImplementedError] {
      Matrix.norm(v, "L1")
    }
    assertThrows[NotImplementedError] {
      Matrix.norm(v, "inf")
    }
    assertThrows[NotImplementedError] {
      Matrix.norm(v, "fro")
    }

    assertThrows[NotImplementedError] {
      Matrix.norm(A)
    }
    assertThrows[NotImplementedError] {
      Matrix.norm(A, "L1")
    }
    assertThrows[NotImplementedError] {
      Matrix.norm(A, "inf")
    }
    assertThrows[NotImplementedError] {
      Matrix.norm(A, "fro")
    }
  }

  test("Matrix[Bitstream] --- rand() test") {
    var A = Matrix.rand[Bitstream](3, 3)

    for (i <- 0 until A.rows; j <- 0 until A.cols) {
      assert((A(i, j) <= 1.0 && A(i, j) >= 0.0) === true)
    }
  }
}