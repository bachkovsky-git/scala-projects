package func.typeclass.eq

import cats.Eq
import cats.instances.int._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._

object EqTest extends App {
  val eqInt = Eq[Int]

  assert(123 === 123)
  assert(123 =!= 234)
  // assert(123 === "123") -- does not compile

  private val mustBeFalse = Option(1) === None
  assert(!mustBeFalse)

  assert(1.some =!= None)
}
