package com.richdougherty.minstrel

import com.richdougherty.minstrel.assemble._
import org.scalatest._

class MinstrelSpec extends UnitSpec {

  "Minstrel" should {

    "run conditionals" in {
      Minstrel.run("""
          : main 1 2 < [ -1 ] [ 1 ] if
        """) should be (-1)
    }

    "run 5!" in {
      Minstrel.run("""
          : factorial ( n -- n! )
          dup 1 <= [ pop 1 ] [
            dup -1 + factorial *
          ] if
          : main 5 factorial
        """) should be (120d)
    }

  }
}
