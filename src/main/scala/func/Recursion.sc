import func.Streams.unfold

def product(xs: List[Double]): Double =
  xs.foldRight(1d)(_ * _)

//A, => B - lazy eval for tail
def foldRight[A, B](as: Seq[A], z: B)(f: (A, => B) => B): B =
  if (as.isEmpty) {
    z
  } else {
    f(as.head, foldRight(as.tail, z)(f))
  }

foldRight(List(1, 2, 3), Nil: List[Int])(_ :: _)

def length[A](as: List[A]): Int =
  foldRight(as, 0)((_, size) => size + 1)

length(List(1, 2, 3))

def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
  as match {
    case Nil     => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

def concat[A](xs: List[List[A]]): List[A] = foldLeft(xs, Nil: List[A])(_ ++ _)
concat(List(1 :: 2 :: 3 :: Nil, 4 :: 5 :: 6 :: Nil, 10 :: 20 :: 30 :: Nil))

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
  (sup, sub) match {
    case (_, Nil)           => true
    case (Nil, _)           => false
    case (x :: xs, s :: ss) => hasSubsequence(xs, if (x == s) ss else sub)
  }

hasSubsequence(Nil, Nil)
hasSubsequence(List(1, 2, 3, 4), Nil)
hasSubsequence(List(1, 2, 3, 4), List(2))
hasSubsequence(List(1, 2, 3, 4), List(1, 2))
hasSubsequence(List(1, 2, 3, 4), List(3, 4))
hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4))
hasSubsequence(List(1, 2, 3, 5, 1, 2, 3, 4), List(2, 3, 4))

hasSubsequence(List(1, 2, 3, 4), List(3, 2))
hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4, 5))

//works with lazy foldRight
def headOption[A](as: Seq[A]): Option[A] =
  foldRight(as, Option.empty[A]) { (a, _) => Some(a) }

assert(headOption(Stream.from(1)).contains(1))
assert(headOption(Stream.empty).isEmpty)



unfold(1)(i => if (i < 5) Some(i, i + 1) else None).toList

def fibs: Stream[BigInt] =
  unfold((BigInt(0), BigInt(1))) { case (p, n) =>
    val next = p + n
    Option(next, (n, next))
  }

fibs.take(10).startsWith(Seq(0, 1))