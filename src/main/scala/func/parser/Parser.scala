package func.parser

import scala.language.implicitConversions

trait Parser[A]

trait ParseError

trait Parsers[ParseError, Parser[+ _]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]

  def char(c: Char): Parser[Char]

  implicit def string(s: String): Parser[String]

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]) : ParserOps[String] =
    ParserOps(f(a))

  implicit class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
}
