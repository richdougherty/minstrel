package com.richdougherty.minstrel

import org.scalatest._
import org.scalatest.matchers.{ Matcher, MatchResult }

class NumberSpec extends UnitSpec {

  def testsFor[T](n: Number[T], zero: T, one: T) = {
    s"converting with $n" should {
      "convert 0 to double" in {
        n.toDouble(zero) should be (0d)
      }
      "convert 0 from double" in {
        n.fromDouble(0d) should be (zero)
      }
      "convert 1 to double" in {
        n.toDouble(one) should be (1d)
      }
      "convert 1 from double" in {
        n.fromDouble(1d) should be (one)
      }
      "convert min to double" in {
        n.toDouble(n.min) should be (n.minAsDouble)
      }
      "convert min from double" in {
        n.fromDouble(n.minAsDouble) should be (n.min)
      }
      "convert max to double" in {
        n.toDouble(n.max) should be (n.maxAsDouble)
      }
      "convert max from double" in {
        n.fromDouble(n.maxAsDouble) should be (n.max)
      }
    }

  }

  "Conversions" when {
    testsFor(I8, 0.toByte, 1.toByte)
    testsFor(U8, 0.toByte, 1.toByte)
    testsFor(I16, 0.toShort, 1.toShort)
    testsFor(U16, 0.toShort, 1.toShort)
    testsFor(I32, 0, 1)
    testsFor(U32, 0, 1)
    testsFor(F32, 0f, 1f)
    testsFor(F64, 0d, 1d)
  }
}
