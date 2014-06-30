package com.richdougherty.minstrel.parse

import com.richdougherty.minstrel._
import org.scalatest._

class ParserSpec extends UnitSpec {

  "Parser" should {
    "parse tiny programs" in {
      Parser.parse(": main 1 2 +") should be (Program(
        Def("main", Num(1), Num(2), Word("+"))
      ))
    }
    "handle newlines" in {
      Parser.parse(": main\n1 2 +") should be (Program(
        Def("main", Num(1), Num(2), Word("+"))
      ))
    }
    "handle leading space" in {
      Parser.parse("  : main\n1 2 +") should be (Program(
        Def("main", Num(1), Num(2), Word("+"))
      ))
    }
    "handle trailing space" in {
      Parser.parse("  : main\n1 2 +  \n ") should be (Program(
        Def("main", Num(1), Num(2), Word("+"))
      ))
    }
    "parse multiple words" in {
      Parser.parse(": main 1 2 sub1 : sub1 sub2 : sub2 +") should be (Program(
        Def("main", Num(1), Num(2), Word("sub1")),
        Def("sub1", Word("sub2")),
        Def("sub2", Word("+"))
      ))
    }
    "parse quotations" in {
      Parser.parse(": main 1 2 < [ -1 ] [ 1 ] if") should be (Program(
        Def("main", Num(1), Num(2), Word("<"), Quot(Num(-1)), Quot(Num(1)), Word("if"))
      ))
    }
  }
}
