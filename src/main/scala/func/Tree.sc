sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def leaf[A](a: A): Tree[A] = Leaf(a)

val tree = Branch(Branch(Leaf(11), Branch(Leaf(2), Leaf(33))), Leaf(4))

def size[A](tree: Tree[A]): Int = tree match {
  case Leaf(_)             => 1
  case Branch(left, right) => size(left) + size(right) + 1
}

assert(size(Leaf(1)) == 1)
assert(size(Branch(Leaf(0), Leaf(1))) == 3)
assert(size(Branch(Leaf(0), Branch(Leaf(0), Leaf(1)))) == 5)
assert(size(Branch(Leaf(0), Branch(Leaf(0), Branch(Leaf(0), Leaf(1))))) == 7)

def maximum(tree: Tree[Int]): Int = tree match {
  case Leaf(n)             => n
  case Branch(left, right) => maximum(left) max maximum(right)
}
assert(maximum(tree) == 33)
assert(maximum(Branch(Leaf(20), Branch(Leaf(0), Branch(Leaf(30), Leaf(1))))) == 30)

def depth[A](tree: Tree[A]): Int = tree match {
  case Leaf(_)             => 1
  case Branch(left, right) => (depth(left) max depth(right)) + 1
}

assert(depth(Leaf(1)) == 1)
assert(depth(tree) == 4)

def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
  val fMap = map(_: Tree[A])(f)

  tree match {
    case Leaf(x)             => Leaf(f(x))
    case Branch(left, right) => Branch(fMap(left), fMap(right))
  }
}
assert(map(Branch(Leaf(1), Leaf(2)))(_ * 2) == Branch(Leaf(2), Leaf(4)))

def fold[A, B](tree: Tree[A])(map: A => B)(reduce: (B, B) => B): B = {
  val subReduce = fold(_: Tree[A])(map)(reduce)
  tree match {
    case Leaf(x)             => map(x)
    case Branch(left, right) => reduce(subReduce(left), subReduce(right))
  }
}

def size1[A](tree: Tree[A]): Int =
  fold(tree) { _ => 1 } { case (l, r) => l + r + 1 }

def maximum1(tree: Tree[Int]): Int =
  fold(tree)(identity)(_ max _)

def depth1[A](tree: Tree[A]): Int =
  fold(tree) { _ => 1 } { (left, right) => (left max right) + 1 }

def map1[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
  fold(tree)(x => leaf(f(x))) { (left, right) => Branch(left, right) }
}

assert (size(tree) == size1(tree))
assert(maximum(tree) == maximum1(tree))
assert(depth(tree) == depth1(tree))
assert(map(tree)("\"" + _.toString + "\"") == map1(tree)("\"" + _.toString + "\""))