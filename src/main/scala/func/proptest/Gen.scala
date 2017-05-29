package func.proptest

import func.Random.Rand
import func.State.sequence
import func.proptest.Prop.{FailedCase, SuccessCount, TestCases}
import func.{Random, State}

import scala.math.abs

object Gen {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(Random.boolean)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = g.listOfN(unit(n))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Random nonNegativeLessThan (stopExclusive - start) map (_ + start))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean flatMap {
      if (_) g1 else g2
    }

  def double: Gen[Double] = Gen(Random.double)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = (g1, g2) match {
    case ((gen1, d1), (gen2, d2)) => double flatMap { d =>
      if (d < d1 / (d1 + d2)) gen1 else gen2
    }
  }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failedCase: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = false
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???
}

case class Prop(run: TestCases => Result)


case class Gen[A](sample: Rand[A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap { n =>
    Gen(sequence(List.fill(n)(sample)))
  }
}