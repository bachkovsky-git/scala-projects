def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

mean(Nil)
mean(1.0 :: 2.0 :: 3.0 :: Nil)

val option = Option.apply(null)

def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs) flatMap { meanXs => mean(xs map { x => math.pow(x - meanXs, 2) }) }
}

def variance1(xs: Seq[Double]): Option[Double] = {
  val varXs = for {
    x <- xs
    meanXs <- mean(xs)
  } yield math.pow(x - meanXs, 2)

  mean(varXs)
}

assert(variance(1.0 :: 2.0 :: 3.0 :: 4.0 :: Nil) == variance1(1.0 :: 2.0 :: 3.0 :: 4.0 :: Nil))

val someInt: Option[Int] = None
val chars: Seq[Char] = Seq('a', 'b', 'c')

val p = for {
  c <- chars
  i <- someInt
} yield {
  (i, c)
}
println(s"p = ${p}")

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
  case (Some(x), Some(y)) => Some(f(x, y))
  case _                  => None
}

def map22[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
  age + numberOfSpeedingTickets / 2

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case _: Exception => None }

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
  val optAge = Try { age.toInt }
  val optTickets = Try { numberOfSpeedingTickets.toInt }
  map22(optAge, optTickets)(insuranceRateQuote)
}

assert(parseInsuranceRateQuote("18", "2").contains(19.0))
assert(parseInsuranceRateQuote("18c", "2").isEmpty)
assert(parseInsuranceRateQuote("18", "2w").isEmpty)
assert(parseInsuranceRateQuote("", "").isEmpty)


def sequence[A](as: List[Option[A]]): Option[List[A]] =
  if (as.forall(_.isDefined)) Some(as.map(_.get)) else None

assert(sequence(Some(1) :: None :: Some(2) :: Nil).isEmpty)
assert(sequence(Some(1) :: Some(3) :: Some(2) :: Nil).isDefined)










































