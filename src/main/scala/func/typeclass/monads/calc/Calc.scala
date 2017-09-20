package func.typeclass.monads.calc

import cats.data.State
import cats.syntax.applicative._

class Calc {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => operator(_ + _)
    case "-" => operator(_ - _)
    case "*" => operator(_ * _)
    case "/" => operator(_ / _)
    case num => operand(num.toInt)
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]) { (state, elem) =>
      state flatMap (_ => evalOne(elem))
    }
  }

  private def operand(num: Int): CalcState[Int] = State[List[Int], Int] {
    stack => (num :: stack, num)
  }

  private def operator(f: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
    case e1::e2::es => (es, f(e1, e2))
  }
}

object Calc extends Calc {
  def main(args: Array[String]): Unit = {
    println(s"args = ${evalOne("42").runA(Nil).value}")

    val res = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans
    println(s"res = ${res.runA(Nil).value}")

    val value = evalAll(List("1", "10", "+")).runA(Nil).value
    println(s"value = ${value}")
  }

}