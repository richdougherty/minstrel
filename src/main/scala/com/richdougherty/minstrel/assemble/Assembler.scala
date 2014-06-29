package com.richdougherty.minstrel.assemble

import com.richdougherty.minstrel.Conversion._
import com.richdougherty.minstrel.Op
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class Assembler {
  var directives = new ArrayBuffer[Directive]

  def initStandard(execStackSize: Int, dataStackSize: Int): Unit = {
    label("exec_top_addr")
    labelRef("exec_top_init")
    label("exec_min_addr")
    labelRef("exec_min")
    label("exec_max_addr")
    labelRef("exec_max")

    label("data_top_addr")
    labelRef("data_top_init")
    label("data_min_addr")
    labelRef("data_min")
    label("data_max_addr")
    labelRef("data_max")

    label("exec_min")
    labelRef("main", size=F64Size)
    label("exec_top_init")
    pad(execStackSize-1, size=F64Size)
    label("exec_max")

    label("data_min")
    label("data_top_init")
    pad(dataStackSize, size=F64Size)
    label("data_max")
  }

  def addDirective(d: Directive): Unit = {
    directives = directives :+ d
  }

  def label(name: String) = addDirective(Label(name))
  def i8(value: Double) = addDirective(Literal(I8Size, value))
  def i32(value: Double) = addDirective(Literal(I32Size, value))
  def f64(value: Double) = addDirective(Literal(F64Size, value))
  def pad(n: Int, size: Size = I8Size) = addDirective(Repeat(n, Literal(size, 0d)))
  def push(value: Double) = {
    addDirective(OpCode(Op.Push))
    addDirective(Literal(F64Size, value))
  }
  def add() = {
    addDirective(OpCode(Op.Add))
  }
  def ret() = {
    addDirective(OpCode(Op.Ret))
  }

  def labelRef(name: String, size: Size = I32Size) = addDirective(LabelRef(size, name))

  def calculateSize: Int = {
    def sizeOf(d: Directive): Int = d match {
      case _: OpCode => 4
      case _: Label => 0
      case Literal(s, _) => s.bytes
      case LabelRef(s, _) => s.bytes
      case Repeat(n, d1) => n * sizeOf(d1)
    }

    directives.foldLeft(0) { case (acc, d) => acc + sizeOf(d) }
  }

  def assemble: Array[Byte] = {
    var buffer = ByteBuffer.allocate(calculateSize)
    buffer.mark()

    // def allocBuffer(needed: Int): Unit = {
    //   if (buffer.remaining < needed) {
    //     val oldBuffer = buffer
    //     buffer = ByteBuffer.allocate(buffer.capacity * 2)
    //     oldBuffer.rewind()
    //     buffer.put(oldBuffer)
    //   }
    // }

    def currentAddr: Int = buffer.position


    val labelAddresses = mutable.Map.empty[String, Int]
    final case class LabelRefHole(addr: Int, size: Size, name: String)
    val labelRefHoles = new ArrayBuffer[LabelRefHole]

    def putValue(s: Size, v: Double): Unit = s match {
      case I8Size =>
        // allocBuffer(1)
        //println(s"pos: ${buffer.position}")
        val b = f64ToI8(v)
        //println(s"putting byte $v->$b")
        buffer.put(b)
        //println(s"wrote value: ${buffer.get(buffer.position-1)}")
        //println(s"pos: ${buffer.position}")
      case I32Size =>
        // allocBuffer(4)
        buffer.putInt(f64ToI32(v))
      case F64Size =>
        // allocBuffer(8)
        buffer.putDouble(v)
    }

    def putDirective(d: Directive): Unit = d match {
      case o: OpCode =>
        putValue(I32Size, o.op.code)
      case Literal(size, value) =>
        putValue(size, value)
      case Repeat(n, d1) =>
        (0 until n).foreach(_ => putDirective(d1))
      case Label(name) =>
        if (labelAddresses.contains(name)) sys.error(s"Duplicate label $name") else {
          labelAddresses(name) = currentAddr
        }
      case LabelRef(size, name) =>
        //println(s"Adding labelRefHole $name")
        labelRefHoles += LabelRefHole(currentAddr, size, name)
        putValue(size, 0) // placeholder
    }

    directives.foreach(putDirective)
    val finalSize = buffer.position
    //println(s"finalSize: ${finalSize}")

    //println(s"labelAddrs: $labelAddresses")
    //println(s"labelRefHoles: $labelRefHoles")

    labelRefHoles.foreach {
      case LabelRefHole(addr, size, name) =>
        val labelAddr = labelAddresses.getOrElse(name, sys.error(s"No label $name"))
        //println(s"Setting labelRef $name at $addr to $labelAddr")
        buffer.position(addr)
        putValue(size, labelAddr)
    }

    val arr = new Array[Byte](finalSize)
    buffer.position(0)
    //println(s"position: ${buffer.position}")
    //println(s"remaining: ${buffer.remaining}")
    buffer.get(arr)
    //println(s"byte 0 of buffer: ${buffer.get(0)}")
    //println(s"byte 0 of arr: ${arr(0)}")
    arr
  }

}