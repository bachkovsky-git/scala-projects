package func.typeclass.monads

import cats.data.Writer
import cats.implicits._
import cats.{Eval, Id, Monad}

import scala.collection.immutable


object Monads extends App {

  /*  trait Monad[F[_]] {
      def pure[A](a: A): F[A]

      def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

      //liftM
      def map[A, B](m: F[A])(f: A => B): F[B] = flatMap(m)(f andThen pure)
    }*/

  // a >>= (x => b.map(y => x*x + y*y))
  def sumSquare[M[_] : Monad](a: M[Int], b: M[Int]): M[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y


  val v1: Option[Int] = sumSquare(Option(3), Option(4))
  val v2: List[Int] = sumSquare(List(1, 2, 3), List(4, 5))
  val v3: Id[Int] = sumSquare(3: Id[Int], 4: Id[Int])

  def pure[A](a: A): Id[A] = a

  def map[A, B](a: Id[A])(f: A => B): Id[B] = f(a)

  def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = map(a)(f)

  val v4: Eval[Int] = sumSquare(Eval.later(2 + 2), Eval.later(2 * 2))

  val saying = Eval.always {
    println("Step 1")
    "The cat"
  }.map { str =>
    println("Step 2")
    s"$str sat on"
  }.memoize.map { str => //memoize: transform Always to Later
    println("Step 3")
    s"$str the mat"
  }

  saying.value
  saying.value

  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial(n - 1).map(_ * n))
    }

  println(factorial(50000).value)
  println(BigInt(1).to(BigInt(50000)).product)


  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    evalFoldRight(as, Eval.now(acc))((a, b) => b.map(fn(a, _))).value
  }

  def evalFoldRight[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case Nil     => acc
    case x :: xs => Eval.defer(fn(x, evalFoldRight(xs, acc)(fn)))
  }

  println(foldRight((1 to 100000).toList, 0)(_ + _))
  println((1 to 100000).sum)


  Writer(Vector(
    "It was the best of times",
    "It was the worst of times"
  ), 123)

  type Logged[A] = Writer[Vector[String], A]

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def slowfactorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowfactorial(n - 1).map(_ * n)
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  val res0: (Vector[String], Int) = slowfactorial(5).run
  println(res0)

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val res1: Seq[(Vector[String], Int)] = Await.result(Future.sequence(Vector(
    Future(slowfactorial(3).run),
    Future(slowfactorial(3).run)
  )), 5.seconds)
  println(res1)
}
