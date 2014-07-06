package com.richdougherty.minstrel.parse

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

object Parser {

  def parse(text: String): Program = {

    sealed trait Token
    final case object DefToken extends Token
    final case object QuotStartToken extends Token
    final case object QuotEndToken extends Token
    final case object CommentStartToken extends Token
    final case object CommentEndToken extends Token
    final case class NumToken(value: Double) extends Token
    final case class WordToken(name: String) extends Token
    final case class RefToken(name: String) extends Token

    var tokens = {
      val strings = text.split("\\s").map(_.trim).filter(!_.isEmpty)
      val rawTokens = strings.map {
        case ":" => DefToken
        case "[" => QuotStartToken
        case "]" => QuotEndToken
        case "(" => CommentStartToken
        case ")" => CommentEndToken
        case s if s.startsWith("@") =>
          s.splitAt(1) match {
            case (_, "") => sys.error("@ references must have a name")
            case (_, name: String) => RefToken(name)
          }
        case s =>
          import java.lang.Double.parseDouble
          try NumToken(parseDouble(s)) catch { case _: NumberFormatException => WordToken(s) }
      }
      val (_, decommentedTokens) = rawTokens.foldLeft((0, Seq.empty[Token])) {
        case ((commentDepth, accTokens), CommentStartToken) =>
          (commentDepth+1, accTokens)
        case ((0, _), CommentEndToken) =>
          sys.error("unexpected comment end token")
        case ((commentDepth, accTokens), CommentEndToken) =>
          (commentDepth-1, accTokens)
        case ((0, accTokens), token) =>
          (0, accTokens :+ token)
        case ((commentDepth, accTokens), _) =>
          (commentDepth, accTokens)
      }
      decommentedTokens.to[List]
    }

    def required[A](expected: String)(f: PartialFunction[Token,A]): A = tokens match {
      case head::rest if f.isDefinedAt(head) =>
        tokens = rest
        f(head)
      case invalid =>
        sys.error("expected $expected: $invalid")
    }

    def optional[A](f: PartialFunction[Token,A]): Option[A] = tokens match {
      case head::rest if f.isDefinedAt(head) =>
        tokens = rest
        Some(f(head))
      case _ =>
        None
    }

    def hasTokens: Boolean = !tokens.isEmpty

    def parseProgram: Program = {
      @tailrec
      def loop(acc: Seq[Def]): Program = {
        if (hasTokens) { loop(acc :+ parseDefStart) } else Program(acc: _*)
      }
      loop(Nil)
    }

    def parseDefStart: Def = required("definition start") {
      case DefToken => parseDefName
    }

    def parseDefName: Def = required("definition name") {
      case WordToken(name) =>
        val body = parseBody
        Def(name, body: _*)
    }

    def parseBody: Seq[AST] = {
      // FIXME: risk of stack overflow
      def loop(acc: Seq[AST]): Seq[AST] = {
        optional {
          case NumToken(value) => loop(acc :+ Num(value))
          case WordToken(name) => loop(acc :+ Word(name))
          case QuotStartToken => loop(acc :+ parseQuot)
        } getOrElse acc
      }
      loop(Nil)
    }

    def parseQuot: Quot = {
      val body = parseBody
      required("quotation end") {
        case QuotEndToken => ()
      }
      Quot(body: _*)
    }

    parseProgram
  }

}