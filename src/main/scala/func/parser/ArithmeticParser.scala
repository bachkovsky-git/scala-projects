package func.parser

import scala.util.parsing.combinator._

class ArithmeticParser extends JavaTokenParsers {
  def expr: Parser[Any] = term ~ rep("+" ~ term | "-" ~ term)

  def term: Parser[Any] = factor ~ rep("*" ~ factor | "/" ~ factor)

  def factor: Parser[Any] = floatingPointNumber | "(" ~ expr ~ ")"
}

object ArithmeticParser extends ArithmeticParser {
  def main(args: Array[String]): Unit = {
    println("input: " + args(0))
    println(parseAll(expr, args(0)))
  }
}
