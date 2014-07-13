package com.richdougherty.minstrel.compile

import com.richdougherty.minstrel._
import com.richdougherty.minstrel.assemble._
import com.richdougherty.minstrel.parse._
import org.scalatest._

class CompilerSpec extends UnitSpec {

  def run(program: Program): Machine = {
    val space = Seq(Label("space"), Repeat(1024, Literal(I8Size, 0)))
    val assembly = StandardHeader.directives(8, 8) ++ Compiler.compile(program) ++ space
    val binary = Assembler.assemble(assembly)
    val machine = new Machine(new Memory(binary))
    machine.run()
    machine
  }

  def result(program: Program): Double = {
    val machine = run(program)
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
    "call words" in {
      result(Program(
        Def("main", Word("subroutine")),
        Def("subroutine", Num(12))
      )) should be (12d)
    }
    "store and reload u8 values" in {
      result(Program(
        Def("main", Ref("space"), Num(33), Word(">u8"), Ref("space"), Word("u8>"))
      )) should be (33d)
    }
    "store and reload i32 values" in {
      result(Program(
        Def("main", Ref("space"), Num(123123123d), Word(">i32"), Ref("space"), Word("i32>"))
      )) should be (123123123d)
    }
    "store and reload f64 values" in {
      result(Program(
        Def("main", Ref("space"), Num(123123123d), Word(">f64"), Ref("space"), Word("f64>"))
      )) should be (123123123d)
    }

    "send 'hello world'" in {
      val helloWorldBytes = "hello world".getBytes("US-ASCII")
      val pushHelloWorld: Vector[AST] = helloWorldBytes.zipWithIndex.to[Vector].flatMap {
        case (byte, index) =>
          Vector(
            Ref("space"), Num(index), Word("+"), // address
            Num(I8.toDouble(byte)), // value
            Word(">i8")
          )
      }
      val m = run(Program(
        Def("main",
          body = pushHelloWorld ++ Vector(
            Ref("space"), Num(helloWorldBytes.length), Word("send")
          )
        )
      ))
      m.outbox.messageCount should be (1)
      m.outbox.popFirst().map(_.content.to[Seq]) should be (Some(helloWorldBytes.to[Seq]))
    }

  }
}
