package com.richdougherty.minstrel

final case class Op(name: String, code: Int)
object Op {
  val Push = Op("push", 0)
  val Pop = Op("pop", 1)
  val Dup = Op("dup", 2)
  val Rot = Op("rot", 3)
  val Jmp = Op("jmp", 4)
  val Call = Op("call", 5)
  val Neg = Op("neg", 6)
  val Bnot = Op("bnot", 7)
  val Not = Op("not", 8)
  val Add = Op("add", 9)
  val Sub = Op("sub", 10)
  val Mul = Op("mul", 11)
  val Div = Op("div", 12)
  val Mod = Op("mod", 13)
  val Bor = Op("bor", 14)
  val Band = Op("band", 15)
  val Bxor = Op("bxor", 16)
  val Shl = Op("shl", 17)
  val Sshr = Op("sshr", 18)
  val Zshr = Op("zshr", 19)
  val Lt = Op("lt", 20)
  val Lte = Op("lte", 21)
  val Gt = Op("gt", 22)
  val Gte = Op("gte", 23)
  val Eq = Op("eq", 24)
  val Ne = Op("ne", 25)
  val Acos = Op("acos", 26)
  val Atan = Op("atan", 27)
  val Cos = Op("cos", 28)
  val Sin = Op("sin", 29)
  val Tan = Op("tan", 30)
  val Ceil = Op("ceil", 31)
  val Floor = Op("floor", 32)
  val Exp = Op("exp", 33)
  val Log = Op("log", 34)
  val Sqrt = Op("sqrt", 35)
  val Abs = Op("abs", 36)
  val Atan2 = Op("atan2", 37)
  val Imul = Op("imul", 38)
  val U8Store = Op("u8store", 39)
  val U8Load = Op("u8load", 40)
  val I8Store = Op("i8store", 41)
  val I8Load = Op("i8load", 42)
  val I16Store = Op("i16store", 43)
  val I16Load = Op("i16load", 44)
  val I32Store = Op("i32store", 45)
  val I32Load = Op("i32load", 46)
  val F32Store = Op("f32store", 47)
  val F32Load = Op("i32load", 48)
  val F64Store = Op("f64store", 49)
  val F64Load = Op("i64load", 50)

  val all = Seq(Push, Pop, Dup, Rot, Jmp, Call, Neg, Bnot, Not, Add, Sub, Mul, Div, Mod, Bor, Band, Bxor, Shl, Sshr, Zshr, Lt, Lte, Gt, Gte, Eq, Ne, Acos, Atan, Cos, Sin, Tan, Ceil, Floor, Exp, Log, Sqrt, Abs, Atan2, Imul, U8Store, U8Load, I8Store, I8Load, I16Store, I16Load, I32Store, I32Load, F32Store, F32Load, F64Store, F64Load)
  val byName: Map[String, Op] = Map(all.map(op => op.name -> op): _*)
  val byCode: Map[Int, Op] = Map(all.map(op => op.code -> op): _*)

}