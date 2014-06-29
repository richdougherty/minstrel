package com.richdougherty.minstrel.compile

import com.richdougherty.minstrel._
import com.richdougherty.minstrel.assemble._
import scala.collection.mutable

object Compiler {
  def compile(program: Program): Seq[Directive] = {
    val builder = new AssemblyBuilder
    val workStack = new mutable.Stack[Def]
    workStack.pushAll(program.defs.reverse)
    val dictionary = StandardImmediates.dictionary

    while (workStack.nonEmpty) {
      val df = workStack.pop()
      builder.label(df.name)
      for (ast <- df.body) ast match {
        case Word(name) =>
          val wordDirectives = dictionary.getOrElse(name, Seq(
            OpCode(Op.Push),
            LabelRef(F64Size, name),
            OpCode(Op.Call)
          ))
          builder.appendAll(wordDirectives)
        case Num(value) =>
          builder.push(value)
        case Quot(body) => ???
      }
      builder.ret()
    }

    builder.directives

  }
}