package com.richdougherty.minstrel.assemble

import com.richdougherty.minstrel.Conversion._
import com.richdougherty.minstrel.Op
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

object Assembler {

  def calculateSize(assembly: Seq[Directive]): Int = {
    def sizeOf(d: Directive): Int = d match {
      case _: OpCode => 4
      case _: Label => 0
      case Literal(s, _) => s.bytes
      case LabelRef(s, _) => s.bytes
      case Repeat(n, d1) => n * sizeOf(d1)
    }

    assembly.foldLeft(0) { case (acc, d) => acc + sizeOf(d) }
  }

  def assemble(assembly: Seq[Directive]): Array[Byte] = {
    val assembledSize = calculateSize(assembly)
    val arr = new Array[Byte](assembledSize)
    var buffer = ByteBuffer.wrap(arr)

    def currentAddr: Int = buffer.position


    val labelAddresses = mutable.Map.empty[String, Int]
    final case class LabelRefHole(addr: Int, size: Size, name: String)
    val labelRefHoles = new ArrayBuffer[LabelRefHole]

    def putValue(s: Size, v: Double): Unit = s match {
      case I8Size =>
        val b = f64ToI8(v)
        buffer.put(b)
      case I32Size =>
        buffer.putInt(f64ToI32(v))
      case F64Size =>
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
        labelRefHoles += LabelRefHole(currentAddr, size, name)
        putValue(size, 0) // placeholder
    }

    assembly.foreach(putDirective)

    labelRefHoles.foreach {
      case LabelRefHole(addr, size, name) =>
        val labelAddr = labelAddresses.getOrElse(name, sys.error(s"No label $name"))
        buffer.position(addr)
        putValue(size, labelAddr)
    }

    arr
  }

}