package com.richdougherty.minstrel.assemble

import com.richdougherty.minstrel.Conversion._
import com.richdougherty.minstrel.Op
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class AssemblyBuilder {
  def directives: Seq[Directive] = buffer
  private val buffer = new ArrayBuffer[Directive]

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

  def append(d: Directive): Unit = {
    buffer += d
  }

  def label(name: String) = append(Label(name))
  def i8(value: Double) = append(Literal(I8Size, value))
  def i32(value: Double) = append(Literal(I32Size, value))
  def f64(value: Double) = append(Literal(F64Size, value))
  def pad(n: Int, size: Size = I8Size) = append(Repeat(n, Literal(size, 0d)))
  def push(value: Double) = {
    append(OpCode(Op.Push))
    append(Literal(F64Size, value))
  }
  def add() = {
    append(OpCode(Op.Add))
  }
  def ret() = {
    append(OpCode(Op.Ret))
  }

  def labelRef(name: String, size: Size = I32Size) = append(LabelRef(size, name))

}