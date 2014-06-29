package com.richdougherty.minstrel.compile

import com.richdougherty.minstrel._
import com.richdougherty.minstrel.assemble._
import org.scalatest._

class CompilerSpec extends UnitSpec {

  import Conversion._

  def result(program: Program): Double = {
    val assembly = StandardHeader.directives(8, 8) ++ Compiler.compile(program)
    val binary = Assembler.assemble(assembly)
    val machine = new Machine(new Memory(binary))
    machine.run()
    machine.data.get
  }

  "Compiled programs" should {
    "add numbers" in {
      result(Program(
        Def("main", Num(1), Num(2), Word("+"))
      )) should be (3d)
    }
    "subtract numbers" in {
      result(Program(
        Def("main", Num(1), Num(2), Word("-"))
      )) should be (-1d)
    }
    "multiply numbers" in {
      result(Program(
        Def("main", Num(15), Num(20), Word("*"))
      )) should be (300d)
    }
    "divide numbers" in {
      result(Program(
        Def("main", Num(1), Num(2), Word("/"))
      )) should be (0.5d)
    }
    "mod numbers" in {
      result(Program(
        Def("main", Num(13), Num(5), Word("%"))
      )) should be (3d)
    }
  }
}
