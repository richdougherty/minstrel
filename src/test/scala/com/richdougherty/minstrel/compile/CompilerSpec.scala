package com.richdougherty.minstrel.compile

import com.richdougherty.minstrel._
import com.richdougherty.minstrel.assemble._
import org.scalatest._

class CompilerSpec extends UnitSpec {

  import Conversion._

  def result(p: Program): Double = {
    val assembly = StandardHeader.directives(8, 8) ++ Compiler.compile(Program(
      Def("main", Num(1), Num(2), Word("+"))
    ))
    val binary = Assembler.assemble(assembly)
    val machine = new Machine(new Memory(binary))
    machine.run()
    machine.data.get
  }

  "Compilers" should {
    "do something" in {
      result(Program(
        Def("main", Num(1), Num(2), Word("+"))
      )) should be (3d)
    }
  }
}
