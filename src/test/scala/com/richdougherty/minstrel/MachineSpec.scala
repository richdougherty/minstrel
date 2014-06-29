package com.richdougherty.minstrel

import com.richdougherty.minstrel.assemble._
import org.scalatest._

class MachineSpec extends UnitSpec {

  import Conversion._

  "Machines" should {
    def createMachine(addDirectives: AssemblyBuilder => Unit): Machine = {
      val assemblyBuilder = new AssemblyBuilder()
      import assemblyBuilder._
      initStandard(32, 32)
      label("main")
      addDirectives(assemblyBuilder)
      val assembled = Assembler.assemble(assemblyBuilder.directives)
      new Machine(new Memory(assembled))
    }

    "add numbers" in {
      val machine = createMachine { assembler =>
        import assembler._
        push(123d)
        push(456d)
        add()
        ret()
      }
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
