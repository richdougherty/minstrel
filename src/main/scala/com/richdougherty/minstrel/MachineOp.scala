package com.richdougherty.minstrel

import scala.annotation.tailrec

object MachineOp {
  object Halt extends MachineOp {
    def step(m: Machine) = sys.error(s"Encountered halt instruction at ${m.exec.get.toInt}")
  }
  object Push extends MachineOp {
    def step(m: Machine) = {
      import m._
      val pc: Int = exec.pop().toInt
      val value: Double = mem.f64Load(pc + 4)
      data.push(value)
      exec.push(pc + 12)
      1
    }
  }
  object Pop extends MachineOp {
    def step(m: Machine) = {
      import m._
      exec.push(exec.pop() + 4)
      data.pop()
      1
    }
  }
  object Dup extends MachineOp {
    def step(m: Machine) = {
      import m._
      exec.push(exec.pop() + 4)
      val a = data.get
      data.push(a)
      1
    }
  }
  val Rot = unimplemented("rot")
  object Ret extends MachineOp {
    def step(m: Machine) = {
      import m._
      exec.pop()
      1
    }
  }
  val Jmp = unimplemented("jmp")
  object Call extends MachineOp {
    def step(m: Machine) = {
      import m._
      val retAddr = exec.pop() + 4
      exec.push(retAddr)
      val callAddr = data.pop()
      exec.push(callAddr)
      1
    }
  }
  object If extends MachineOp {
    def step(m: Machine) = {
      import m._
      val nextAddr = exec.pop().toInt + 4
      val f = I32.fromDouble(data.pop())
      val t = I32.fromDouble(data.pop())
      val cond = I32.fromDouble(data.pop())
      val branch = if (cond == 0) f else t
      exec.push(nextAddr)
      exec.push(branch)
      1
    }
  }
  val Neg = UnaryMachineOp.f64((a: Double) => -a)
  val Bnot = UnaryMachineOp.i32((a: Int) => ~a)
  val Not = UnaryMachineOp.i1((a: Boolean) => !a)
  val Add = BinaryMachineOp.f64((a: Double, b: Double) => a + b)
  val Sub = BinaryMachineOp.f64((a: Double, b: Double) => a - b)
  val Mul = BinaryMachineOp.f64((a: Double, b: Double) => a * b)
  val Div = BinaryMachineOp.f64((a: Double, b: Double) => a / b)
  val Mod = BinaryMachineOp.f64((a: Double, b: Double) => a % b)
  val Bor = BinaryMachineOp.i32((a: Int, b: Int) => a | b)
  val Band = BinaryMachineOp.i32((a: Int, b: Int) => a & b)
  val Bxor = BinaryMachineOp.i32((a: Int, b: Int) => a ^ b)
  val Shl = BinaryMachineOp.i32((a: Int, b: Int) => a << b)
  val Sshr = BinaryMachineOp.i32((a: Int, b: Int) => a >> b)
  val Zshr = BinaryMachineOp.i32((a: Int, b: Int) => a >>> b)
  val Lt = BinaryMachineOp.cmp((a: Double, b: Double) => a < b)
  val Lte = BinaryMachineOp.cmp((a: Double, b: Double) => a <= b)
  val Gt = BinaryMachineOp.cmp((a: Double, b: Double) => a > b)
  val Gte = BinaryMachineOp.cmp((a: Double, b: Double) => a >= b)
  val Eq = BinaryMachineOp.cmp((a: Double, b: Double) => a == b)
  val Ne = BinaryMachineOp.cmp((a: Double, b: Double) => a != b)
  val Acos = UnaryMachineOp.f64((a: Double) => Math.acos(a))
  val Atan = UnaryMachineOp.f64((a: Double) => Math.atan(a))
  val Cos = UnaryMachineOp.f64((a: Double) => Math.cos(a))
  val Sin = UnaryMachineOp.f64((a: Double) => Math.sin(a))
  val Tan = UnaryMachineOp.f64((a: Double) => Math.tan(a))
  val Ceil = UnaryMachineOp.f64((a: Double) => Math.ceil(a))
  val Floor = UnaryMachineOp.f64((a: Double) => Math.floor(a))
  val Exp = UnaryMachineOp.f64((a: Double) => Math.exp(a))
  val Log = UnaryMachineOp.f64((a: Double) => Math.log(a))
  val Sqrt = UnaryMachineOp.f64((a: Double) => Math.sqrt(a))
  val Abs = UnaryMachineOp.f64((a: Double) => Math.abs(a))
  val Atan2 = BinaryMachineOp.f64((a: Double, b: Double) => Math.atan2(a, b))
  val Imul = BinaryMachineOp.i32((a: Int, b: Int) => a * b)
  val U8Store = unimplemented("u8store")
  val U8Load = unimplemented("u8load")
  val I8Store = unimplemented("i8store")
  val I8Load = unimplemented("i8load")
  val I16Store = unimplemented("i16store")
  val I16Load = unimplemented("i16load")
  val I32Store = unimplemented("i32store")
  val I32Load = unimplemented("i32load")
  val F32Store = unimplemented("f32store")
  val F32Load = unimplemented("f32load")
  val F64Store = unimplemented("f64store")
  val F64Load = unimplemented("f64load")
  private def unimplemented(name: String) = new MachineOp {
    def step(m: Machine): Int = sys.error(s"op $name not implemented")
  }
  val byOp: Map[Op, MachineOp] = Map(Op.Halt -> Halt, Op.Push -> Push, Op.Pop -> Pop, Op.Dup -> Dup, Op.Rot -> Rot, Op.Ret -> Ret, Op.Jmp -> Jmp, Op.Call -> Call, Op.If -> If, Op.Neg -> Neg, Op.Bnot -> Bnot, Op.Not -> Not, Op.Add -> Add, Op.Sub -> Sub, Op.Mul -> Mul, Op.Div -> Div, Op.Mod -> Mod, Op.Bor -> Bor, Op.Band -> Band, Op.Bxor -> Bxor, Op.Shl -> Shl, Op.Sshr -> Sshr, Op.Zshr -> Zshr, Op.Lt -> Lt, Op.Lte -> Lte, Op.Gt -> Gt, Op.Gte -> Gte, Op.Eq -> Eq, Op.Ne -> Ne, Op.Acos -> Acos, Op.Atan -> Atan, Op.Cos -> Cos, Op.Sin -> Sin, Op.Tan -> Tan, Op.Ceil -> Ceil, Op.Floor -> Floor, Op.Exp -> Exp, Op.Log -> Log, Op.Sqrt -> Sqrt, Op.Abs -> Abs, Op.Atan2 -> Atan2, Op.Imul -> Imul, Op.U8Store -> U8Store, Op.U8Load -> U8Load, Op.I8Store -> I8Store, Op.I8Load -> I8Load, Op.I16Store -> I16Store, Op.I16Load -> I16Load, Op.I32Store -> I32Store, Op.I32Load -> I32Load, Op.F32Store -> F32Store, Op.F32Load -> F32Load, Op.F64Store -> F64Store, Op.F64Load -> F64Load)
}

trait MachineOp {
  def step(m: Machine): Int
}

object UnaryMachineOp {
  def f64(f: Double => Double) = new UnaryMachineOp(f)
  def i32(f: Int => Int) = UnaryMachineOp.f64 { ad: Double =>
    val a: Int = I32.fromDouble(ad)
    val b: Int = f(a)
    val bd: Double = I32.toDouble(b)
    bd
  }
  def i1(f: Boolean => Boolean) = UnaryMachineOp.f64 { ad: Double =>
    val ai: Int = I32.fromDouble(ad)
    val a: Boolean = ai != 0
    val b: Boolean = f(a)
    val bi: Int = if (b) 1 else 0
    val bd: Double = I32.toDouble(bi)
    bd
  }
}

final class UnaryMachineOp(f: Double => Double) extends MachineOp {
  def step(m: Machine): Int = {
    import m._
    val pc: Int = exec.pop().toInt
    val a = data.pop()
    val b = f(a)
    data.push(b)
    exec.push(pc + 4)
    1
  }
}

object BinaryMachineOp {
  def f64(f: (Double,Double) => Double) = new BinaryMachineOp(f)
  def i32(f: (Int,Int) => Int) = BinaryMachineOp.f64 { (ad: Double, bd: Double) =>
    val a: Int = I32.fromDouble(ad)
    val b: Int = I32.fromDouble(bd)
    val c: Int = f(a, b)
    val cd: Double = I32.toDouble(c)
    cd
  }
  def cmp(f: (Double,Double) => Boolean) = BinaryMachineOp.f64 { (a: Double, b: Double) =>
    val c: Boolean = f(a, b)
    val ci: Int = if (c) 1 else 0
    val cd: Double = I32.toDouble(ci)
    cd
  }
}
final class BinaryMachineOp(f: (Double,Double) => Double) extends MachineOp {
  def step(m: Machine): Int = {
    import m._
    val pc: Int = exec.pop().toInt
    val b = data.pop()
    val a = data.pop()
    val c = f(a, b)
    data.push(c)
    exec.push(pc + 4)
    1
  }
}
