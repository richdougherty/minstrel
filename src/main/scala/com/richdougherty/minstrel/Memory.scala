package com.richdougherty.minstrel

class Memory(raw: Array[Byte]) {
  def this(memSize: Int) = this(new Array[Byte](memSize))

  def size: Int = raw.length

  private val buffer = java.nio.ByteBuffer.wrap(raw)

  def u32Load(addr: Int): Double = {
    U32.toDouble(buffer.getInt(addr))
  }
  def u32Store(addr: Int, value: Double): Unit = {
    buffer.putInt(addr, U32.fromDouble(value))
  }
  def i32Load(addr: Int): Double = {
    I32.toDouble(buffer.getInt(addr))
  }
  def i32Store(addr: Int, value: Double): Unit = {
    buffer.putInt(addr, I32.fromDouble(value))
  }
  def f64Load(addr: Int): Double = {
    buffer.getDouble(addr)
  }
  def f64Store(addr: Int, value: Double): Unit = {
    buffer.putDouble(addr, value)
  }
}
