package com.richdougherty.minstrel

import scala.collection.JavaConversions._

final case class Message(content: Array[Byte]) {
  def size = content.size
}
object Message {
  def apply(bytes: Byte*): Message = Message(bytes.toArray)
}

class Mailbox {
  private val deque = new java.util.ArrayDeque[Message]

  def messageCount: Int = deque.size
  def totalBytesUsed: Int = deque.foldLeft(0) {
    case (acc, m) => acc + m.content.length + 4
  }
  def pushFirst(m: Message): Unit = deque.addFirst(m)
  def popFirst(): Option[Message] = Option(deque.pollFirst())
  def sizeFirst: Option[Int] = Option(deque.peekFirst).map(_.size)
  def pushLast(m: Message): Unit = deque.addLast(m)
}