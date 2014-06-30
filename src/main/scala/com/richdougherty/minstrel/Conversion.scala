package com.richdougherty.minstrel

import java.nio.ByteBuffer

object Conversion {
  val u32MaxU32: Int = 0xffffffff
  val i32MaxU32: Int = Integer.MAX_VALUE
  val i32MaxI32: Int = Integer.MAX_VALUE
  val i32MinI32: Int = Integer.MIN_VALUE
  val u32MaxF64: Double = Math.pow(2, 32)-1
  val i32MaxF64: Double = Math.pow(2, 31)-1
  val i32MinF64: Double = -1*Math.pow(2, 31)

  def f64ToI1(d: Double): Boolean = {
    if (d == 0d) false else if (d == 1d) true else sys.error("$d is not a boolean value")
  }
  def i1ToF64(b: Boolean): Double = {
    if (b) 1d else 0d
  }
  def i1ToBytes(b: Boolean): Array[Byte] = {
    val arr = new Array[Byte](1)
    arr(0) = if (b) 1 else 0
    arr
  }
  def f64ToI8(d: Double): Byte = {
    d.toByte
  }
  def i8ToF64(b: Byte): Double = {
    b.toDouble
  }
  def i8ToBytes(b: Byte): Array[Byte] = {
    val arr = new Array[Byte](1)
    arr(0) = b
    arr
  }
  def f64ToI32(d: Double): Int = {
    d.toInt
  }
  def i32ToF64(i: Int): Double = {
    i.toDouble
  }
  def i32ToBytes(i: Int): Array[Byte] = {
    val arr = new Array[Byte](4)
    val buffer = ByteBuffer.wrap(arr)
    buffer.putInt(i)
    arr
  }
  def f64ToU32(d: Double): Int = {
    d.toLong.toInt
  }
  def f64ToBytes(d: Double): Array[Byte] = {
    val arr = new Array[Byte](8)
    val buffer = ByteBuffer.wrap(arr)
    buffer.putDouble(d)
    arr
  }
  def u32ToF64(i: Int): Double = {
    (i & 0xffffffffl).toDouble
  }
  def u32ToI64(i: Int): Long = {
    (i & 0xffffffffl)
  }
  def numDisplay(d: Double): String = {
    val l = d.toLong
    if (l.toDouble == d) l.toString else d.toString
  }
}