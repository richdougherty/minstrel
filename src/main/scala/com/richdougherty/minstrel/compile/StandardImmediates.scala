package com.richdougherty.minstrel.compile

import com.richdougherty.minstrel._
import com.richdougherty.minstrel.assemble._
import scala.collection.immutable.Seq

object StandardImmediates {

  val dictionary: Map[String, Seq[Directive]] = Map[String, Seq[Directive]](
    "halt" -> Seq(OpCode(Op.Halt)),
    "pop" -> Seq(OpCode(Op.Pop)),
    "dup" -> Seq(OpCode(Op.Dup)),
    "rot" -> Seq(OpCode(Op.Rot)),
    "ret" -> Seq(OpCode(Op.Ret)),
    "jmp" -> Seq(OpCode(Op.Jmp)),
    "call" -> Seq(OpCode(Op.Call)),
    "neg" -> Seq(OpCode(Op.Neg)),
    "~" -> Seq(OpCode(Op.Bnot)),
    "!" -> Seq(OpCode(Op.Not)),
    "+" -> Seq(OpCode(Op.Add)),
    "-" -> Seq(OpCode(Op.Sub)),
    "*" -> Seq(OpCode(Op.Mul)),
    "/" -> Seq(OpCode(Op.Div)),
    "%" -> Seq(OpCode(Op.Mod)),
    "|" -> Seq(OpCode(Op.Bor)),
    "&" -> Seq(OpCode(Op.Band)),
    "^" -> Seq(OpCode(Op.Bxor)),
    "<<" -> Seq(OpCode(Op.Shl)),
    ">>" -> Seq(OpCode(Op.Sshr)),
    ">>>" -> Seq(OpCode(Op.Zshr)),
    "<" -> Seq(OpCode(Op.Lt)),
    "<=" -> Seq(OpCode(Op.Lte)),
    ">" -> Seq(OpCode(Op.Gt)),
    ">=" -> Seq(OpCode(Op.Gte)),
    "==" -> Seq(OpCode(Op.Eq)),
    "!=" -> Seq(OpCode(Op.Ne)),
    "acos" -> Seq(OpCode(Op.Acos)),
    "atan" -> Seq(OpCode(Op.Atan)),
    "cos" -> Seq(OpCode(Op.Cos)),
    "sin" -> Seq(OpCode(Op.Sin)),
    "tan" -> Seq(OpCode(Op.Tan)),
    "ceil" -> Seq(OpCode(Op.Ceil)),
    "floor" -> Seq(OpCode(Op.Floor)),
    "exp" -> Seq(OpCode(Op.Exp)),
    "log" -> Seq(OpCode(Op.Log)),
    "sqrt" -> Seq(OpCode(Op.Sqrt)),
    "abs" -> Seq(OpCode(Op.Abs)),
    "atan2" -> Seq(OpCode(Op.Atan2)),
    "imul" -> Seq(OpCode(Op.Imul)),
    ">u8" -> Seq(OpCode(Op.U8Store)),
    "u8>" -> Seq(OpCode(Op.U8Load)),
    ">i8" -> Seq(OpCode(Op.I8Store)),
    "i8>" -> Seq(OpCode(Op.I8Load)),
    ">i16" -> Seq(OpCode(Op.I16Store)),
    "i16>" -> Seq(OpCode(Op.I16Load)),
    ">i32" -> Seq(OpCode(Op.I32Store)),
    "i32>" -> Seq(OpCode(Op.I32Load)),
    ">f32" -> Seq(OpCode(Op.F32Store)),
    "f32>" -> Seq(OpCode(Op.F32Load)),
    ">f64" -> Seq(OpCode(Op.F64Store)),
    "f64>" -> Seq(OpCode(Op.F64Load))
  )

}