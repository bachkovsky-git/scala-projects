package func.proptest

import func.Random.Rand
import func.State.sequence
import func.Streams.unfold
import func.proptest.Prop.{FailedCase, SuccessCount, TestCases}
import func.{Random, State}

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

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zipWithIndex take n map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      } find (_.isFalsified) getOrElse Passed
  }

  def randomStream[A](g: Gen[A])(rng: Random): Stream[A] =
    unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

case class Prop(run: (TestCases, Random) => Result) {
  def &&(p: Prop): Prop = Prop { (n, rng) =>
    Prop.this.run(n, rng) match {
      case f: Falsified => f
      case _            => p run(n, rng)
    }
  }

  def ||(p: Prop): Prop = Prop { (n, rng) =>
    Prop.this.run(n, rng) match {
      case Passed => Passed
      case _      => p run(n, rng)
    }
  }
}


case class Gen[A](sample: Rand[A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] = size flatMap { n =>
    Gen(sequence(List.fill(n)(sample)))
  }
}

case class SGen[+A](forSize: Int => Gen[A])