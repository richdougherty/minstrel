package com.richdougherty.minstrel

import org.scalatest._
import org.scalatest.matchers.{ Matcher, MatchResult }

class ConversionSpec extends UnitSpec {

  import Conversion._

  "Conversions" when {
    "converting from u32 to f64" should {
      "convert 0" in {
        Conversion.u32ToF64(0) should be (0d)
      }
      "convert 1" in {
        Conversion.u32ToF64(1) should be (1d)
      }
      "convert i32 max from int to double" in {
        Conversion.u32ToF64(i32MaxU32) should be (i32MaxF64)
      }
      "convert u32 max from int to double" in {
        Conversion.u32ToF64(u32MaxU32) should be (u32MaxF64)
      }
    }
    "converting from f64 to u32" should {
      "convert 0" in {
        Conversion.f64ToU32(0d) should be (0)
      }
      "convert 1" in {
        Conversion.f64ToU32(1d) should be (1)
      }
      "convert i32 max" in {
        Conversion.f64ToU32(i32MaxF64) should be (i32MaxU32)
      }
      "convert u32 max" in {
        Conversion.f64ToU32(u32MaxF64) should be (u32MaxU32)
      }
    }
    "converting from i32 to f64" should {
      "convert 0 from int to double" in {
        Conversion.i32ToF64(0) should be (0d)
      }
      "convert 1" in {
        Conversion.i32ToF64(1) should be (1d)
      }
      "convert -1" in {
        Conversion.i32ToF64(1) should be (1d)
      }
      "convert i32 max from int to double" in {
        Conversion.i32ToF64(i32MaxI32) should be (i32MaxF64)
      }
      "convert i32 min from int to double" in {
        Conversion.i32ToF64(i32MinI32) should be (i32MinF64)
      }
    }
    "converting from f64 to i32" should {
      "convert 0" in {
        Conversion.f64ToI32(0d) should be (0)
      }
      "convert 1" in {
        Conversion.f64ToI32(1d) should be (1)
      }
      "convert -1" in {
        Conversion.f64ToI32(1d) should be (1)
      }
      "convert i32 max" in {
        Conversion.f64ToI32(i32MaxF64) should be (i32MaxI32)
      }
      "convert i32 min" in {
        Conversion.f64ToU32(i32MinF64) should be (i32MinI32)
      }
    }
  }
}
