package com.richdougherty.minstrel

import org.scalatest._

class MailboxSpec extends UnitSpec {

  "Mailbox" should {

    "work" in {
      val mbox = new Mailbox
      mbox.messageCount should be(0)
      mbox.totalBytesUsed should be(0)
      mbox.sizeFirst should be(None)
      mbox.popFirst() should be(None)
      mbox.pushFirst(Message(1, 2, 3))
      mbox.messageCount should be(1)
      mbox.totalBytesUsed should be(7)
      mbox.sizeFirst should be(Some(3))
      mbox.popFirst().map(_.content.to[Seq]) should be(Some(Seq[Byte](1, 2, 3)))
      mbox.pushFirst(Message(1, 2, 3))
      mbox.messageCount should be(1)
      mbox.totalBytesUsed should be(7)
      mbox.pushLast(Message(4, 5, 6))
      mbox.messageCount should be(2)
      mbox.totalBytesUsed should be(14)
      mbox.pushFirst(Message(7, 8, 9, 0))
      mbox.messageCount should be(3)
      mbox.totalBytesUsed should be(22)
      mbox.sizeFirst should be(Some(4))
      mbox.popFirst().map(_.content.to[Seq]) should be(Some(Seq[Byte](7, 8, 9, 0)))
      mbox.messageCount should be(2)
      mbox.totalBytesUsed should be(14)
      mbox.sizeFirst should be(Some(3))
      mbox.popFirst().map(_.content.to[Seq]) should be(Some(Seq[Byte](1, 2, 3)))
      mbox.messageCount should be(1)
      mbox.totalBytesUsed should be(7)
      mbox.sizeFirst should be(Some(3))
      mbox.popFirst().map(_.content.to[Seq]) should be(Some(Seq[Byte](4, 5, 6)))
      mbox.messageCount should be(0)
      mbox.totalBytesUsed should be(0)
      mbox.sizeFirst should be(None)
    }
  }
}
