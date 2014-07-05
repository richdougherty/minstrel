package com.richdougherty.minstrel

import java.nio.ByteBuffer

sealed trait Number[T] {
  def min: T
  def max: T
  def minAsDouble: Double
  def maxAsDouble: Double
  def toDouble(value: T): Double
  def fromDouble(value: Double): T
  def byteSize: Int
  def put(bb: ByteBuffer, value: T)
  def get(bb: ByteBuffer): T
  def put(bb: ByteBuffer, pos: Int, value: T)
  def get(bb: ByteBuffer, pos: Int): T
  def toBytes(value: T): Array[Byte] = {
    val arr = new Array[Byte](byteSize)
    val bb = ByteBuffer.wrap(arr)
    put(bb, value)
    arr
  }
  def toString(value: T): String = {
    val d = toDouble(value)
    val l = d.toLong
    if (l.toDouble == d) l.toString else d.toString
  }
}

final case object I8 extends Number[Byte] {
  val min: Byte = -128.toByte
  val max: Byte = 127.toByte
  val minAsDouble: Double = -1*Math.pow(2, 7)
  val maxAsDouble: Double = Math.pow(2, 7)-1

  def toDouble(b: Byte): Double = b.toDouble
  def fromDouble(d: Double): Byte = d.toByte
  def byteSize = 1
  def put(bb: ByteBuffer, b: Byte): Unit = bb.put(b)
  def get(bb: ByteBuffer): Byte = bb.get()
  def put(bb: ByteBuffer, pos: Int, b: Byte): Unit = bb.put(pos, b)
  def get(bb: ByteBuffer, pos: Int): Byte = bb.get(pos)
}

final case object U8 extends Number[Byte] {
  val min: Byte = 0.toByte
  val max: Byte = 255.toByte
  val minAsDouble: Double = 0d
  val maxAsDouble: Double = Math.pow(2, 8)-1

  def toDouble(b: Byte): Double = (b & 0xff).toDouble
  def fromDouble(d: Double): Byte = d.toByte
  def byteSize = 1
  def put(bb: ByteBuffer, b: Byte): Unit = bb.put(b)
  def get(bb: ByteBuffer): Byte = bb.get()
  def put(bb: ByteBuffer, pos: Int, b: Byte): Unit = bb.put(pos, b)
  def get(bb: ByteBuffer, pos: Int): Byte = bb.get(pos)
}

final case object I16 extends Number[Short] {
  val min: Short = -128.toShort
  val max: Short = 127.toShort
  val minAsDouble: Double = -1*Math.pow(2, 7)
  val maxAsDouble: Double = Math.pow(2, 7)-1

  def toDouble(s: Short): Double = s.toDouble
  def fromDouble(d: Double): Short = d.toShort
  def byteSize = 1
  def put(bb: ByteBuffer, s: Short): Unit = bb.putShort(s)
  def get(bb: ByteBuffer): Short = bb.getShort()
  def put(bb: ByteBuffer, pos: Int, s: Short): Unit = bb.putShort(pos, s)
  def get(bb: ByteBuffer, pos: Int): Short = bb.getShort(pos)
}

final case object U16 extends Number[Short] {
  val min: Short = 0.toShort
  val max: Short = 255.toShort
  val minAsDouble: Double = 0d
  val maxAsDouble: Double = Math.pow(2, 8)-1

  def toDouble(s: Short): Double = (s & 0xffff).toDouble
  def fromDouble(d: Double): Short = d.toShort
  def byteSize = 1
  def put(bb: ByteBuffer, s: Short): Unit = bb.putShort(s)
  def get(bb: ByteBuffer): Short = bb.getShort()
  def put(bb: ByteBuffer, pos: Int, s: Short): Unit = bb.putShort(pos, s)
  def get(bb: ByteBuffer, pos: Int): Short = bb.getShort(pos)
}

final case object I32 extends Number[Int] {
  val min: Int = Integer.MIN_VALUE
  val max: Int = Integer.MAX_VALUE
  val minAsDouble: Double = -1*Math.pow(2, 31)
  val maxAsDouble: Double = Math.pow(2, 31)-1

  def toDouble(i: Int): Double = i.toDouble
  def fromDouble(d: Double): Int = d.toInt
  def byteSize = 4
  def put(bb: ByteBuffer, i: Int): Unit = bb.putInt(i)
  def get(bb: ByteBuffer): Int = bb.getInt()
  def put(bb: ByteBuffer, pos: Int, i: Int): Unit = bb.putInt(pos, i)
  def get(bb: ByteBuffer, pos: Int): Int = bb.getInt(pos)
}

final case object U32 extends Number[Int] {
  val min: Int = 0
  val max: Int = 0xffffffff
  val minAsDouble: Double = 0d
  val maxAsDouble: Double = Math.pow(2, 32)-1

  def toDouble(i: Int): Double = (i & 0xffffffffl).toDouble
  def fromDouble(d: Double): Int = d.toLong.toInt
  def byteSize = 4
  def put(bb: ByteBuffer, i: Int): Unit = bb.putInt(i)
  def get(bb: ByteBuffer): Int = bb.getInt()
  def put(bb: ByteBuffer, pos: Int, i: Int): Unit = bb.putInt(pos, i)
  def get(bb: ByteBuffer, pos: Int): Int = bb.getInt(pos)
}

final case object F32 extends Number[Float] {
  val min: Float = java.lang.Float.NEGATIVE_INFINITY
  val max: Float = java.lang.Float.POSITIVE_INFINITY
  val minAsDouble: Double = java.lang.Double.NEGATIVE_INFINITY
  val maxAsDouble: Double = java.lang.Double.POSITIVE_INFINITY

  def toDouble(f: Float): Double = f.toDouble
  def fromDouble(d: Double): Float = d.toFloat
  def byteSize = 4
  def put(bb: ByteBuffer, f: Float): Unit = bb.putFloat(f)
  def get(bb: ByteBuffer): Float = bb.getFloat()
  def put(bb: ByteBuffer, pos: Int, f: Float): Unit = bb.putFloat(pos, f)
  def get(bb: ByteBuffer, pos: Int): Float = bb.getFloat(pos)
}

final case object F64 extends Number[Double] {
  val min: Double = java.lang.Double.NEGATIVE_INFINITY
  val max: Double = java.lang.Double.POSITIVE_INFINITY
  val minAsDouble: Double = java.lang.Double.NEGATIVE_INFINITY
  val maxAsDouble: Double = java.lang.Double.POSITIVE_INFINITY

  def toDouble(d: Double): Double = d
  def fromDouble(d: Double): Double = d
  def byteSize = 8
  def put(bb: ByteBuffer, d: Double): Unit = bb.putDouble(d)
  def get(bb: ByteBuffer): Double = bb.getDouble()
  def put(bb: ByteBuffer, pos: Int, d: Double): Unit = bb.putDouble(pos, d)
  def get(bb: ByteBuffer, pos: Int): Double = bb.getDouble(pos)
}