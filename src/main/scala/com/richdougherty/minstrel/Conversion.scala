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

  def f64ToI1(d: Double): Boolean = U1.fromDouble(d)
  def i1ToF64(b: Boolean): Double = U1.toDouble(b)
  def i1ToBytes(b: Boolean): Array[Byte] = U1.toBytes(b)
  def f64ToI8(d: Double): Byte = I8.fromDouble(d)
  def i8ToF64(b: Byte): Double = I8.toDouble(b)
  def i8ToBytes(b: Byte): Array[Byte] = I8.toBytes(b)
  def f64ToI32(d: Double): Int = I32.fromDouble(d)
  def i32ToF64(i: Int): Double = I32.toDouble(i)
  def i32ToBytes(i: Int): Array[Byte] = I32.toBytes(i)
  def f64ToU32(d: Double): Int = U32.fromDouble(d)
  def f64ToBytes(d: Double): Array[Byte] = F64.toBytes(d)
  def u32ToF64(i: Int): Double = U32.toDouble(i)
  def numDisplay(d: Double): String = F64.toString(d)
}