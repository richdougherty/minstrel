package com.richdougherty.minstrel.assemble

import com.richdougherty.minstrel.Conversion._
import com.richdougherty.minstrel.Op
import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

class AssemblyBuilder {
  def directives: Seq[Directive] = buffer
  private val buffer = new ArrayBuffer[Directive]

  def append(d: Directive): Unit = {
    buffer += d
  }
  def appendAll(ds: Seq[Directive]): Unit = {
    buffer ++= ds
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