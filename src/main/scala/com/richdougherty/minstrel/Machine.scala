package com.richdougherty.minstrel

import scala.annotation.tailrec

final class Machine(val mem: Memory) {
  def this(memSize: Int) = this(new Memory(memSize))

  import Machine._

  val exec = new Stack(mem, ExecAddr)
  val data = new Stack(mem, DataAddr)
  val inbox = new Mailbox
  val outbox = new Mailbox
  var state: MachineState = Running

  def step(log: Boolean = false): StepResult = {
    if (exec.isEmpty) StepResult(0, Some(IO.Halt)) else {
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
      assert(state == Running)
      val stepResult = step(log)
      val cyclesAfterStep = cyclesSoFar + stepResult.cycles
      stepResult.io match {
        case None => run0(cyclesAfterStep)
        case Some(IO.Halt) =>
          state = Halted
          cyclesAfterStep
        case Some(IO.InSize) =>
          val size = inbox.sizeFirst.getOrElse(0)
          data.push(I32.toDouble(size))
          run0(cyclesAfterStep)
        case Some(IO.InRead(memAddr, msgOffset, length)) =>
          val msg = inbox.popFirst().get
          for (i <- 0 until msg.content.size) {
            val x: Byte = msg.content(i)
            mem.store(I8, memAddr+i, x)
          }
          run0(cyclesAfterStep)
        case Some(IO.OutWrite(memAddr, length)) =>
          val content = new Array[Byte](length)
          for (i <- 0 until length) {
            val x: Byte = mem.load(I8, memAddr+i)
            content(i) = x
          }
          outbox.pushLast(Message(content))
          run0(cyclesAfterStep)
        case Some(IO.InWait) =>
          state = WaitingForInput
          cyclesAfterStep
      }
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

  final case class StepResult(cycles: Int, io: Option[IO])

  sealed trait MachineState
  case object Running extends MachineState
  case object WaitingForInput extends MachineState
  case object Halted extends MachineState

  sealed trait IO
  object IO {
    final case object Halt extends IO
    final case object InSize extends IO
    final case object InWait extends IO
    final case class InRead(memAddr: Int, msgOffset: Int, length: Int) extends IO
    final case class OutWrite(memAddr: Int, length: Int) extends IO
  }

}
