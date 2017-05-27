

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

def TryOpt[A](a: => A): Option[A] =
  try Some(a)
  catch { case _: Exception => None }

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
  val optAge = TryOpt { age.toInt }
  val optTickets = TryOpt { numberOfSpeedingTickets.toInt }
  map22(optAge, optTickets)(insuranceRateQuote)
}

assert(parseInsuranceRateQuote("18", "2").contains(19.0))
assert(parseInsuranceRateQuote("18c", "2").isEmpty)
assert(parseInsuranceRateQuote("18", "2w").isEmpty)
assert(parseInsuranceRateQuote("", "").isEmpty)

/**
  * short-circuiting traverse
  */
def traverse[A, B](as: Seq[A])(f: A => Option[B]): Option[Seq[B]] = Some {
  as.foldLeft(Seq.empty[B]) { (seq, el) =>
    f(el) match {
      case None    => return None
      case Some(x) => seq ++ Seq(x)
    }
  }
}

//works on infinite streams
val ints: Stream[Int] = Stream(1, 2) #::: ints
assert(traverse(ints) { i => if (i % 2 == 0) Some(i) else None }.isEmpty)

//also works on finite sequences
assert(Seq("1", "2", "3") == traverse(Seq(1, 2, 3)) { i => Some(i.toString) }.get)

def sequence[A](as: Seq[Option[A]]): Option[Seq[A]] =
  traverse(as) { identity }

//val inf: Stream[Option[Int]] = Stream.continually(Seq(Some(1), Some(2), None)).flatten
val inf: Stream[Option[Int]] = Stream(Some(1), Some(2), None) #::: inf
assert(sequence(inf).isEmpty)
assert(sequence(Some(1) :: None :: Some(2) :: Nil).isEmpty)
assert(sequence(Some(1) :: Some(3) :: Some(2) :: Nil).isDefined)

trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: (Nothing) => B): Either[E, B] = this

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))

  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this

  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for (be <- b) yield f(value, be)
}

def TryEither[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }

def parseInsuranceRateQuote1(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] =
  for {
    ageInt  <- TryEither { age.toInt }
    tickets <- TryEither { numberOfSpeedingTickets.toInt }
  } yield insuranceRateQuote(ageInt, tickets)

def traverseE[E, A, B](as: Seq[A])(f: A => Either[E, B]): Either[E, Seq[B]] = Right {
  as.foldLeft(Seq.empty[B]) { (seq, el) =>
    f(el) match {
      case l: Left[E] => return l
      case Right(v)   => seq ++ Seq(v)
    }
  }
}

assert(traverseE(ints) { i => if (i % 2 == 0) Right(i) else Left("sos!") } == Left("sos!"))

def sequenceE[E, A](es: Seq[Either[E, A]]): Either[E, Seq[A]] =
  traverseE(es) { identity }

val infE: Stream[Either[String, Int]] = Stream(Right(1), Right(2), Left("kek")) #::: infE
assert(sequenceE(infE) == Left("kek"))
assert(sequenceE(Right(1) :: Left("kek") :: Right(2) :: Nil) == Left("kek"))
assert(sequenceE(Seq(Right(1), Right(3), Right(2))) == Right(Seq(1, 3, 2)))
