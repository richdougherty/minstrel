package com.richdougherty.minstrel.compile

import com.richdougherty.minstrel._
import com.richdougherty.minstrel.assemble._
import com.richdougherty.minstrel.parse._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable

object Compiler {
  def flatten(program: Program): Program = {
    val flatDefs = new ArrayBuffer[Def]
    def flattenDef(df: Def): Unit = {
      var anonId = 0
      var newBody = new ArrayBuffer[AST]
      var quotDefs = new ArrayBuffer[Def]
      for (ast <- df.body) ast match {
        case Quot(body) =>
          val quotName = df.name+"-"+anonId
          anonId += 1
          quotDefs += Def(quotName, body)
          newBody += QuotWord(quotName)
        case other => newBody += other
      }
      flatDefs += df.copy(body = newBody.to[immutable.Seq])
      quotDefs.foreach(flattenDef)
    }
    program.defs.foreach(flattenDef)
    program.copy(defs = flatDefs.to[immutable.Seq])
  }

  def compile(program: Program): Seq[Directive] = {
    val builder = new AssemblyBuilder
    val dictionary = StandardImmediates.dictionary
    for (df <- flatten(program).defs) {
      builder.label(df.name)
      for (ast <- df.body) ast match {
        case Word(name) =>
          val wordDirectives = dictionary.getOrElse(name, Seq(
            OpCode(Op.Push),
            LabelRef(F64Size, name),
            OpCode(Op.Call)
          ))
          builder.appendAll(wordDirectives)
        case QuotWord(name) =>
          builder.appendAll(Seq(
            OpCode(Op.Push),
            LabelRef(F64Size, name)
          ))
        case Num(value) =>
          builder.push(value)
        case Quot(body) => sys.error("quot not expected in flattened program")
        case Ref(name) =>
          builder.appendAll(Seq(
            OpCode(Op.Push),
            LabelRef(F64Size, name)
          ))
      }
      builder.ret()
    }

    builder.directives
  }

}