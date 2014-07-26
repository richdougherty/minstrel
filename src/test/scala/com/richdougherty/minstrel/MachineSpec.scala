package com.richdougherty.minstrel

import com.richdougherty.minstrel.assemble._
import org.scalatest._

class MachineSpec extends UnitSpec {

  "Machines" should {
    def createMachine(addDirectives: AssemblyBuilder => Unit): Machine = {
      val assemblyBuilder = new AssemblyBuilder()
      import assemblyBuilder._
      StandardHeader.build(assemblyBuilder, 32, 32)
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
      import Machine._
      machine.step() should be (StepResult(1, None))
      machine.data.get should be (123d)
      machine.step() should be (StepResult(1, None))
      machine.data.get should be (456d)
      machine.step() should be (StepResult(1, None))
      machine.data.get should be (579d)
      machine.step() should be (StepResult(1, None))
      machine.step() should be (StepResult(0, Some(IO.Halt)))
    }
  }
}
