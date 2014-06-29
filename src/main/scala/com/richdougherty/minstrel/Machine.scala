package com.richdougherty.minstrel

import scala.annotation.tailrec

final class Machine(val mem: Memory) {
  def this(memSize: Int) = this(new Memory(memSize))

  import Machine._
  val exec = new Stack(mem, ExecAddr)
  val data = new Stack(mem, DataAddr)

  def step(): Int = {
    if (exec.isEmpty) 0 else {
      val pc: Int = exec.get.toInt
      val opCode: Int = mem.i32Load(pc).toInt
      val op: Op = Op.byCode.get(opCode).getOrElse(sys.error(s"Unknown opCode $opCode at $pc"))
      op match {
        case Op.Halt => {
          sys.error(s"Encountered halt instruction at $pc")
        }
        case Op.Push => {
          val pc: Int = exec.pop().toInt
          val value: Double = mem.f64Load(pc + 4)
          data.push(value)
          exec.push(pc + 12)
          1
        }
        case Op.Add => {
          val pc: Int = exec.pop().toInt
          val b = data.pop()
          val a = data.pop()
          val c = a + b
          data.push(c)
          exec.push(pc + 4)
          1
        }
        case Op.Ret => {
          exec.pop()
          1
        }
        case _ => sys.error(s"$op not implemented yet")
      }
    }
  }

  def run(): Int = {
    @tailrec
    def run0(cyclesSoFar: Int): Int = {
      val cycles = step()
      if (cycles == 0) cyclesSoFar else run0(cycles + cyclesSoFar)
    }
    run0(0)
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
