package com.richdougherty.minstrel

import Conversion._

class Memory(memSize: Int) {

  private val raw = new Array[Byte](memSize)
  private val buffer = java.nio.ByteBuffer.wrap(raw)

  def u32Load(addr: Int): Double = {
    u32ToF64(buffer.getInt(addr))
  }
  def u32Store(addr: Int, value: Double): Unit = {
    buffer.putInt(addr, f64ToU32(value))
  }
  def i32Load(addr: Int): Double = {
    i32ToF64(buffer.getInt(addr))
  }
  def i32Store(addr: Int, value: Double): Unit = {
    buffer.putInt(addr, f64ToI32(value))
  }
  def f64Load(addr: Int): Double = {
    buffer.getDouble(addr)
  }
  def f64Store(addr: Int, value: Double): Unit = {
    buffer.putDouble(addr, value)
  }
}
