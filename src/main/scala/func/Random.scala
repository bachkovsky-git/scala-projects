package func

import func.State.{map2, sequence}

import scala.math.{abs, max}

trait Random {
  def nextInt: (Int, Random)
}

case class SimpleRandom(seed: Long) extends Random {
  override def nextInt: (Int, Random) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRandom = SimpleRandom(newSeed)
    ((newSeed >>> 16).toInt, newRandom)
  }
}

object Random {
  type Rand[+A] = State[Random, A]

  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt flatMap { i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) State {
      (mod, _)
    }
    else nonNegativeLessThan(n)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def int: Rand[Int] = State(_.nextInt)

  def boolean: Rand[Boolean] = int map (_ % 2 == 0)

  def nonNegativeInt: Rand[Int] = int map {
    case (Int.MinValue) => Int.MaxValue
    case (i)            => abs(i)
  }

  def nonNegativeEven: Rand[Int] =
    nonNegativeInt map { i => i - i % 2 }

  def double: Rand[Double] =
    nonNegativeInt map { i => max(0.0, i - 1) / Int.MaxValue }

  def intDouble: Rand[(Int, Double)] = both(int, double)

  def doubleInt: Rand[(Double, Int)] = both(double, int)

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))
}