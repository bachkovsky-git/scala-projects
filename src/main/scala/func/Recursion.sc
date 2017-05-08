def product(xs: List[Double]): Double =
  xs.foldRight(1d)(_ * _)

def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil     => z
    case x :: xs => f(x, foldRight(xs, z)(f))
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