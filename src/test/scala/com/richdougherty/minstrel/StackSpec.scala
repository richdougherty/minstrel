package com.richdougherty.minstrel

import org.scalatest._

class StackSpec extends UnitSpec {

  import Conversion._

  "Stacks" should {
    def createStack: Stack = {
      val mem = new Memory(36)
      val stack = new Stack(mem, 0)
      stack.init(12, 24)
      stack
    }

    "work" in {
      val stack = createStack
      stack.seq should be (Seq())
      stack.isEmpty should be (true)
      an[Exception] should be thrownBy stack.get
      an[Exception] should be thrownBy stack.pop()
      stack.push(12345d)
      stack.seq should be (Seq(12345d))
      stack.get should be(12345d)
      stack.pop() should be (12345d)
      stack.seq should be (Seq())
      stack.isEmpty should be (true)
      stack.push(1d)
      stack.seq should be (Seq(1d))
      stack.push(2d)
      stack.seq should be (Seq(1d, 2d))
      stack.push(3d)
      stack.seq should be (Seq(1d, 2d, 3d))
      stack.pop() should be (3d)
      stack.seq should be (Seq(1d, 2d))
      stack.pop() should be (2d)
      stack.seq should be (Seq(1d))
      stack.pop() should be (1d)
      stack.seq should be (Seq())
      stack.isEmpty should be (true)

      stack.push(-1d)
      stack.push(-2d)
      stack.push(-3d)
      an[Exception] should be thrownBy stack.push(-4d)
    }
  }
}
