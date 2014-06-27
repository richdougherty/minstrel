package com.richdougherty.minstrel

class Stack(mem: Memory, baseAddr: Int) {
  // points to next allocation, top is just before this address
  val top = new FixnumCell(mem, baseAddr)
  val min = new FixnumCell(mem, baseAddr+4)
  val max = new FixnumCell(mem, baseAddr+8)

  def init(addr: Int, size: Int): Unit = {
    top.set(addr)
    min.set(addr)
    max.set(addr + size)
  }

  def isEmpty: Boolean = {
    return top.get == min.get
  }

  // get the top value
  def get: Double = {
    val addr = top.get - 8
    if (addr < min.get) sys.error("stack underflow: $addr < ${min.get}")
    mem.f64Load(addr)
  }
  // set the top value
  def set(value: Double) = {
    val addr = top.get
    if (addr > max.get) sys.error(s"stack overflow: $addr > ${max.get}")
    mem.f64Store(addr - 8, value)
  }

  def push(value: Double) = {
    top.set(top.get + 8)
    set(value)
  }
  def pop() = {
    val value = get
    top.set(top.get - 8)
    value
  }
}