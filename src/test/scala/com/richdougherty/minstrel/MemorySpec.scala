package com.richdougherty.minstrel

import org.scalatest._

class MemorySpec extends UnitSpec {

  "Memory" should {
    "work with u32s" in {
      val mem = new Memory(128)
      mem.u32Store(0, U32.max)
      mem.u32Load(0) should be (U32.maxAsDouble)
      mem.u32Store(10, -1) // negative number becomes positive
      mem.u32Load(10) should be (U32.maxAsDouble)
    }
    "work with i32s" in {
      val mem = new Memory(128)
      mem.i32Store(0, 1)
      mem.i32Load(0) should be (1d)
      mem.i32Store(10, -1999)
      mem.i32Load(10) should be (-1999d)
    }
    "work with f64s" in {
      val mem = new Memory(128)
      mem.f64Store(0, 1d)
      mem.f64Load(0) should be (1d)
      mem.f64Store(10, -1999d)
      mem.f64Load(10) should be (-1999d)
    }
  }
}
