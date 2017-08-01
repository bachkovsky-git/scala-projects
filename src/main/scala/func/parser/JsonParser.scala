package func.parser

import scala.util.parsing.combinator.JavaTokenParsers

class JsonParser extends JavaTokenParsers {
  def value: Parser[Any] = (
        obj
      | arr
      | stringLiteral
      | floatingPointNumber ^^ (_.toDouble)
      | "null" ^^ (x => null)
      | "true" ^^ (x => true)
      | "false" ^^ (x => false)
    )

  def obj: Parser[Map[String, Any]] =
    "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)

  def arr: Parser[List[Any]] =
    "[" ~> repsep(value, ",") <~ "]"

  def member: Parser[(String, Any)] =
    stringLiteral ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name, value) }

}

object JsonParser extends JsonParser {
  def main(args: Array[String]): Unit = {
    val json =
      """{
        |  "address book": {
        |    "name": "John Smith",
        |    "address": {
        |      "street": "10 Market Street",
        |      "city" : "San Francisco, CA",
        |      "zip"
        |      : 94111
        |    },
        |    "phone numbers": [
        |      "408 338-4238",
        |      "408 111-6892"
        |    ]
        |  }
        |}
        |""".stripMargin
    println(parseAll(value, json))
  }
}
