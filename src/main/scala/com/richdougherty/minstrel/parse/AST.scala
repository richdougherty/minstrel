package com.richdougherty.minstrel.parse

import scala.collection.immutable

final case class Def(name: String, body: immutable.Seq[AST])
object Def {
  def apply(name: String, asts: AST*): Def = Def(name, immutable.Seq(asts: _*))
}

sealed trait AST
final case class Word(name: String) extends AST
final case class Quot(body: immutable.Seq[AST]) extends AST
object Quot {
  def apply(asts: AST*): Quot = Quot(immutable.Seq(asts: _*))
}
final case class QuotWord(name: String) extends AST
final case class Num(value: Double) extends AST
final case class Ref(name: String) extends AST

final case class Program(defs: immutable.Seq[Def])
object Program {
  def apply(defs: Def*): Program = Program(immutable.Seq(defs: _*))
}