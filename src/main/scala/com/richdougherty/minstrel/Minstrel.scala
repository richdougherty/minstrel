package com.richdougherty.minstrel

import com.richdougherty.minstrel.assemble._
import com.richdougherty.minstrel.compile._
import com.richdougherty.minstrel.parse._

object Minstrel {

  def run(source: String, execStackSize: Int = 256, dataStackSize: Int = 256, log: Boolean = false): Double = {
    val ast = Parser.parse(source)
    val assembly = StandardHeader.directives(execStackSize, dataStackSize) ++ Compiler.compile(ast)
    val binary = Assembler.assemble(assembly)
    val machine = new Machine(new Memory(binary))
    machine.run(log = log)
    machine.data.get
  }

}