package com.richdougherty.minstrel

import org.scalatest._

class MachineSpec extends UnitSpec {

  import Conversion._

  "Machines" should {
    def createMachine: Machine = {
      val assembler = new assemble.Assembler()
      import assembler._
      initStandard(8, 8)
      label("main")
      push(123d)
      push(456d)
      add()
      ret()
      new Machine(new Memory(assemble))
    }

    "add numbers" in {
      val machine = createMachine
      machine.step should be (1)
      machine.data.get should be (123d)
      machine.step should be (1)
      machine.data.get should be (456d)
      machine.step should be (1)
      machine.data.get should be (579d)
      machine.step should be (1)
      machine.step should be (0)
    }
  }
}
