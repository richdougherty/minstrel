package com.richdougherty.minstrel

class FixnumCell(mem: Memory, addr: Int) {
  def get: Int = {
    val i = mem.i32Load(addr).toInt
    assert(i >= 0)
    i
  }
  def set(i: Int) = {
    assert(i >= 0)
    mem.i32Store(addr, i)
  }
}