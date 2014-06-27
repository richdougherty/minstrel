package com.richdougherty.minstrel

import org.scalatest._

class MachineSpec extends UnitSpec {

  import Conversion._

  "Machines" should {
    def createMachine: Machine = {
      val machine = new Machine(256)

      var free = 24
      def alloc(size: Int): Int = {
        val addr = free
        free += size
        addr
      }

      machine.exec.init(alloc(64), 64)
      machine.data.init(alloc(64), 64)

      machine.exec.push(free)
      machine.mem.i32Store(alloc(4), Op.Push.code)
      machine.mem.f64Store(alloc(8), 123d)
      machine.mem.i32Store(alloc(4), Op.Push.code)
      machine.mem.f64Store(alloc(8), 456d)
      machine.mem.i32Store(alloc(4), Op.Add.code)
      machine
    }

    "add numbers" in {
      val machine = createMachine
      machine.step should be (1)
      machine.data.get should be (123d)
      machine.step should be (1)
      machine.data.get should be (456d)
      machine.step should be (1)
      machine.data.get should be (579d)
    }
  }
}
