package com.richdougherty.minstrel.assemble

import com.richdougherty.minstrel._
import org.scalatest._

class AssemblerSpec extends UnitSpec {

  import Conversion._

  implicit class Hexable(bytes: Array[Byte]) {
    def toHex: String = {
      val sb = new StringBuilder(bytes.length * 2)
      for (byte <- bytes) {
        val i = byte & 0xff
        if (i < 16) sb.append('0')
        sb.append(Integer.toHexString(i))
      }
      sb.toString
    }
  }

  "Assemblers" should {
    "know the size of i8 literals" in {
      val assembler = new Assembler()
      import assembler._
      i8(1)
      calculateSize should be (1)
    }
    "assemble i8 literals" in {
      val assembler = new Assembler()
      import assembler._
      i8(1)
      assemble.toHex should be ("01")
    }
    "know the size of i32 literals" in {
      val assembler = new Assembler()
      import assembler._
      i32(0x1234567)
      calculateSize should be (4)
    }
    "assemble i32 literals" in {
      val assembler = new Assembler()
      import assembler._
      i32(0x1234567)
      assemble.toHex should be ("01234567")
    }
    "know the size of f64 literals" in {
      val assembler = new Assembler()
      import assembler._
      f64(Math.PI)
      calculateSize should be (8)
    }
    "assemble f64 literals" in {
      val assembler = new Assembler()
      import assembler._
      f64(Math.PI)
      assemble.toHex should be ("400921fb54442d18")
    }
    "assemble labels" in {
      val assembler = new Assembler()
      import assembler._
      labelRef("x")
      label("x")
      assemble.toHex should be ("00000004")
    }
    "assemble push" in {
      val assembler = new Assembler()
      import assembler._
      push(123)
      assemble.toHex should be (i32ToBytes(Op.Push.code).toHex ++ f64ToBytes(123).toHex)
    }
    "start exec at main" in {
      val assembler = new Assembler()
      import assembler._
      initStandard(4, 4)
      label("main")
      val mem = new Memory(assemble)
      val machine = new Machine(mem)
      machine.exec.min.get should be (24)
      machine.exec.max.get should be (56)
      machine.exec.top.get should be (32)
      machine.data.min.get should be (56)
      machine.data.max.get should be (88)
      machine.data.top.get should be (56)
      machine.exec.get should be (mem.size) // location of main
    }
  }
}
