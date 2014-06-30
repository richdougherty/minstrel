package com.richdougherty.minstrel

import scala.annotation.tailrec

final class Machine(val mem: Memory) {
  def this(memSize: Int) = this(new Memory(memSize))

  import Machine._
  val exec = new Stack(mem, ExecAddr)
  val data = new Stack(mem, DataAddr)

  def step(log: Boolean = false): Int = {
    if (exec.isEmpty) 0 else {
      val pc: Int = exec.get.toInt
      val opCode: Int = mem.i32Load(pc).toInt
      val op: Op = Op.byCode.getOrElse(opCode, sys.error(s"Unknown opCode $opCode at $pc"))
      if (log) {
        println(op.name)
        println(data.seq.mkString("-> ", " ", ""))
      }
      val machineOp: MachineOp = MachineOp.byOp.getOrElse(op, sys.error(s"No MachineOp for $op"))
      machineOp.step(this)
    }
  }

  def run(log: Boolean = false): Int = {
    @tailrec
    def run0(cyclesSoFar: Int): Int = {
      val cycles = step(log)
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
