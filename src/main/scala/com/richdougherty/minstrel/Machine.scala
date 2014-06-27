package com.richdougherty.minstrel

class Machine(memSize: Int) {
  import Machine._
  val mem = new Memory(memSize)
  val exec = new Stack(mem, ExecAddr)
  val data = new Stack(mem, DataAddr)

  def step(): Int = {
    if (exec.isEmpty) 0 else {
      val pc: Int = exec.get.toInt
      val opCode: Int = mem.i32Load(pc).toInt
      val op: Op = Op.byCode.get(opCode).getOrElse(sys.error(s"Unknown opCode $opCode at $pc"))
      op match {
        case Op.Push => {
          val value: Double = mem.f64Load(pc + 4)
          data.push(value)
          exec.push(pc + 12)
          1
        }
        case Op.Add => {
          val b = data.pop()
          val a = data.pop()
          val c = a + b
          data.push(c)
          exec.push(pc + 4)
          1
        }
        case _ => sys.error(s"Op $opCode not implemented yet")
      }
    }
  }
}

object Machine {
  private var free: Int = 0
  private def alloc(size: Int): Int = {
    val addr = free
    free += size
    addr
  }

  val ExecAddr = alloc(12)
  val DataAddr = alloc(12)
}
