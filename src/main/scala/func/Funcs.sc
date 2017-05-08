def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
  (b: B) => f(a, b)

val partial = partial1(1, (a: Any, b: Any) => a == b)
partial(2)
partial(1)

def sum(a: Int, b: Int) = a + b
val partSum1 = sum(1, _: Int)
val partSum2 = sum(_: Int, 2)
val partSum3: (Int) => Int = sum(_, 3)

val sumF = (x: Int, y: Int) => x + y
val sumF1: (Int, Int) => Int = _ + _
val sumF2 = (_: Int) + (_: Int)

def curry[A, B, C](f: (A, B) => C): A => B => C =
  (a: A) => (b: B) => f(a, b)

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))
//  f compose g

lazy val fibs: Stream[BigInt] =
  BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { case (a, b) => a + b }
fibs(1000)
