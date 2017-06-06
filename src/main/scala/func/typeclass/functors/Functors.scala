package func.typeclass.functors

import cats.Functor
import cats.instances.function._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.functor._


object Functors extends App {
  private val f1: (Int) => Double = x => x * math.Pi
  private val f2: (Double) => String = x => x.toString
  private val f3: (Int) => String = f1 map f2

  println(f3(42))

  private val l1 = List(1, 2, 3)
  private val l2 = Functor[List].map(l1) {
    _ * 2 + 2
  }

  private val resOpt: Option[String] = Functor[Option].lift(f3)(Option(42))


  sealed trait Tree[+A]

  object Tree {

    private final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    private final case class Leaf[A](value: A) extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

    def leaf[A](value: A): Tree[A] = Leaf(value)

    implicit object TreeFunctor extends Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
        case Leaf(v)      => leaf(f(v))
        case Branch(l, r) => branch(map(l)(f), map(r)(f))
      }
    }

  }

  import func.typeclass.functors.Functors.Tree._

  val tree = branch(branch(leaf(1), branch(leaf(2), leaf(3))), branch(branch(leaf(4), branch(leaf(5), leaf(6))), leaf(7)))
  val tree1 = tree map (_ * 10 - 1)
  println(s"tree1 = $tree1")

}
