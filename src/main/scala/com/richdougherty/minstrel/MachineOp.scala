package com.richdougherty.minstrel

import scala.annotation.tailrec

import Conversion._

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
  val Pop = unimplemented("pop")
  val Dup = unimplemented("dup")
  val Rot = unimplemented("rot")
  object Ret extends MachineOp {
    def step(m: Machine) = {
      import m._
      exec.pop()
      1
    }
  }
  val Jmp = unimplemented("jmp")
  val Call = unimplemented("call")
  val Neg = unimplemented("neg")
  val Bnot = unimplemented("bnot")
  val Not = unimplemented("not")
  val Add = BinaryMachineOp.f64((a: Double, b: Double) => a + b)
  val Sub = BinaryMachineOp.f64((a: Double, b: Double) => a - b)
  val Mul = BinaryMachineOp.f64((a: Double, b: Double) => a * b)
  val Div = BinaryMachineOp.f64((a: Double, b: Double) => a / b)
  val Mod = BinaryMachineOp.f64((a: Double, b: Double) => a % b)
  val Bor = BinaryMachineOp.i32((a: Int, b: Int) => a | b)
  val Band = BinaryMachineOp.i32((a: Int, b: Int) => a & b)
  val Bxor = BinaryMachineOp.i32((a: Int, b: Int) => a ^ b)
  val Shl = unimplemented("shl")
  val Sshr = unimplemented("sshr")
  val Zshr = unimplemented("zshr")
  val Lt = unimplemented("lt")
  val Lte = unimplemented("lte")
  val Gt = unimplemented("gt")
  val Gte = unimplemented("gte")
  val Eq = unimplemented("eq")
  val Ne = unimplemented("ne")
  val Acos = unimplemented("acos")
  val Atan = unimplemented("atan")
  val Cos = unimplemented("cos")
  val Sin = unimplemented("sin")
  val Tan = unimplemented("tan")
  val Ceil = unimplemented("ceil")
  val Floor = unimplemented("floor")
  val Exp = unimplemented("exp")
  val Log = unimplemented("log")
  val Sqrt = unimplemented("sqrt")
  val Abs = unimplemented("abs")
  val Atan2 = unimplemented("atan2")
  val Imul = unimplemented("imul")
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
  val byOp: Map[Op, MachineOp] = Map(Op.Halt -> Halt, Op.Push -> Push, Op.Pop -> Pop, Op.Dup -> Dup, Op.Rot -> Rot, Op.Ret -> Ret, Op.Jmp -> Jmp, Op.Call -> Call, Op.Neg -> Neg, Op.Bnot -> Bnot, Op.Not -> Not, Op.Add -> Add, Op.Sub -> Sub, Op.Mul -> Mul, Op.Div -> Div, Op.Mod -> Mod, Op.Bor -> Bor, Op.Band -> Band, Op.Bxor -> Bxor, Op.Shl -> Shl, Op.Sshr -> Sshr, Op.Zshr -> Zshr, Op.Lt -> Lt, Op.Lte -> Lte, Op.Gt -> Gt, Op.Gte -> Gte, Op.Eq -> Eq, Op.Ne -> Ne, Op.Acos -> Acos, Op.Atan -> Atan, Op.Cos -> Cos, Op.Sin -> Sin, Op.Tan -> Tan, Op.Ceil -> Ceil, Op.Floor -> Floor, Op.Exp -> Exp, Op.Log -> Log, Op.Sqrt -> Sqrt, Op.Abs -> Abs, Op.Atan2 -> Atan2, Op.Imul -> Imul, Op.U8Store -> U8Store, Op.U8Load -> U8Load, Op.I8Store -> I8Store, Op.I8Load -> I8Load, Op.I16Store -> I16Store, Op.I16Load -> I16Load, Op.I32Store -> I32Store, Op.I32Load -> I32Load, Op.F32Store -> F32Store, Op.F32Load -> F32Load, Op.F64Store -> F64Store, Op.F64Load -> F64Load)
}

trait MachineOp {
  def step(m: Machine): Int
}
object BinaryMachineOp {
  def f64(f: (Double,Double) => Double): BinaryMachineOp = new BinaryMachineOp(f)
  def i32(f: (Int,Int) => Int): BinaryMachineOp = BinaryMachineOp.f64 { (ad: Double, bd: Double) =>
    val a: Int = f64ToI32(ad)
    val b: Int = f64ToI32(bd)
    val c: Int = f(a, b)
    val cd: Double = i32ToF64(c)
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
