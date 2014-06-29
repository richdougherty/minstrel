package com.richdougherty.minstrel.assemble

object StandardHeader {

  def directives(execStackSize: Int, dataStackSize: Int): Seq[Directive] = {
    val builder = new AssemblyBuilder
    build(builder, execStackSize, dataStackSize)
    builder.directives
  }

  def build(builder: AssemblyBuilder, execStackSize: Int, dataStackSize: Int): Unit = {
    import builder._
    label("exec_top_addr")
    labelRef("exec_top_init")
    label("exec_min_addr")
    labelRef("exec_min")
    label("exec_max_addr")
    labelRef("exec_max")

    label("data_top_addr")
    labelRef("data_top_init")
    label("data_min_addr")
    labelRef("data_min")
    label("data_max_addr")
    labelRef("data_max")

    label("exec_min")
    labelRef("main", size=F64Size)
    label("exec_top_init")
    pad(execStackSize-1, size=F64Size)
    label("exec_max")

    label("data_min")
    label("data_top_init")
    pad(dataStackSize, size=F64Size)
    label("data_max")
  }

}