package com.richdougherty.minstrel.assemble

import com.richdougherty.minstrel.Op

sealed trait Directive
final case class Label(name: String) extends Directive
final case class OpCode(op: Op) extends Directive
final case class Literal(size: Size, value: Double) extends Directive
final case class LabelRef(size: Size, name: String) extends Directive
final case class Repeat(n: Int, dir: Directive) extends Directive

sealed trait Size { def bytes: Int }
final case object I8Size extends Size { def bytes = 1 }
final case object I32Size extends Size { def bytes = 4 }
final case object F64Size extends Size { def bytes = 8 }
