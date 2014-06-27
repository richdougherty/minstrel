package com.richdougherty.minstrel

object Conversion {
  val u32MaxU32: Int = 0xffffffff
  val i32MaxU32: Int = Integer.MAX_VALUE
  val i32MaxI32: Int = Integer.MAX_VALUE
  val i32MinI32: Int = Integer.MIN_VALUE
  val u32MaxF64: Double = Math.pow(2, 32)-1
  val i32MaxF64: Double = Math.pow(2, 31)-1
  val i32MinF64: Double = -1*Math.pow(2, 31)

  def f64ToI32(d: Double): Int = {
    d.toInt
  }
  def i32ToF64(i: Int): Double = {
    i.toDouble
  }
  def f64ToU32(d: Double): Int = {
    d.toLong.toInt
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