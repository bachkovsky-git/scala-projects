package func.typeclass.monads.state

import cats.data.State
import cats.syntax.applicative._

import scala.math.{abs, max}

trait Random {
  def nextInt: (Random, Int)
}

object Random {
  type Rand[A] = State[Random, A]

  def apply(): Random = SimpleRandom(System.currentTimeMillis())

  private case class SimpleRandom(seed: Long) extends Random {
    override def nextInt: (Random, Int) = {
      val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextState = SimpleRandom(nextSeed)
      (nextState, (nextSeed >>> 16).toInt)
    }
  }

  def int: Rand[Int] = State(_.nextInt)

  def nonNegativeInt: Rand[Int] = int map {
    case (Int.MinValue) => Int.MaxValue
    case (i)            => abs(i)
  }

  def double: Rand[Double] =
    nonNegativeInt map { i => max(0.0, i - 1) / Int.MaxValue }


  def ints(count: Int): Rand[List[Int]] = {
    val list = List.fill(count)(int)
    val init = State.pure[Random, List[Int]](Nil)
    list.foldLeft(init) { (state, elem) =>
      elem.map2(state)(_ :: _)
    }
  }

  def main(args: Array[String]): Unit = {
    val threeRandom = for {
      i <- Random.int
      d <- Random.double
      l <- Random.ints(3)
    } yield (i, d, l)

    val initialState = Random()
    val result = threeRandom.runA(initialState).value
    println(s"result = $result")
  }
}


