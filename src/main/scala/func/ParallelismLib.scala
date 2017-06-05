package func

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}

import func.ParallelismLib.Par.toParOps

import scala.language.implicitConversions


object ParallelismLib extends App {

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def run[A](service: ExecutorService)(parallelComputation: Par[A]): Future[A] = parallelComputation(service)

    def unit[A](a: A): Par[A] = _ => UnitFuture(a)

    /* This version respects timeouts. See `Map2Future` below. */
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
      val (af, bf) = (a(es), b(es))
      Map2Future(af, bf, f)
    }

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] =
      map2(a, b) { (a, b) => map(c) { cc => f(a, b, cc) } } flatMap identity

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] =
      map3(a, b, c) { (a, b, c) => map(d) { dd => f(a, b, c, dd) } } flatMap identity

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      (pa map2 unit()) ((a, _) => f(a))

    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => {
      val a = pa runIn es get()
      f(a) runIn es
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = delay {
      val asyncFs = ps map asyncF(f)
      sequence(asyncFs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val fs = as map asyncF(a => if (f(a)) Some(a) else None)
      sequence(fs) map (_.flatten)
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(List[A]())) { (p, pl) => (p map2 pl) (_ :: _) }

    def delay[A](a: => Par[A]): Par[A] =
      es => a(es)

    def lazyUnit[A](a: => A): Par[A] = delay(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      cond flatMap { if (_) t else f }

    def choiceN[A](idx: Par[Int])(choices: List[Par[A]]): Par[A] =
      idx flatMap (choices(_))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      key flatMap choices


    def join[A](a: Par[Par[A]]): Par[A] = a flatMap identity

    private case class UnitFuture[A](get: A) extends Future[A] {
      override def isDone: Boolean = true

      override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

      override def isCancelled: Boolean = false

      override def get(timeout: Long, unit: TimeUnit): A = get
    }

    private case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                           f: (A, B) => C) extends Future[C] {
      @volatile var cache: Option[C] = None

      def isDone: Boolean = cache.isDefined

      def isCancelled: Boolean = a.isCancelled || b.isCancelled

      def cancel(evenIfRunning: Boolean): Boolean =
        a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

      def get: C = compute(Long.MaxValue)

      def get(timeout: Long, units: TimeUnit): C =
        compute(TimeUnit.NANOSECONDS.convert(timeout, units))

      private def compute(timeoutInNanos: Long): C = cache match {
        case Some(c) => c
        case None    =>
          val start = System.nanoTime
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime
          val aTime = stop - start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }
    }

    /* Gives us infix syntax for `Par`. */
    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A]) {
      def runIn(es: ExecutorService): Future[A] = Par.run(es)(p)

      def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, pb)(f)

      def map[B](f: A => B): Par[B] = Par.map(p)(f)

      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
    }

    implicit def toParOps[A](p: List[A]): ListParOps[A] = ListParOps(p)

    case class ListParOps[A](p: List[A]) {
      def parMap[B](f: A => B): Par[List[B]] = Par.parMap(p)(f)
    }

    def fold[A](seq: IndexedSeq[A], empty: A, append: (A, A) => A): Par[A] = {
      if (seq.size <= 1) {
        unit(seq.headOption getOrElse empty)
      } else {
        val (l, r) = seq.splitAt(seq.length / 2)
        val leftFork = delay(fold(l, empty, append))
        val rightFork = delay(fold(r, empty, append))

        (leftFork map2 rightFork) (append)
      }
    }

    def mapReduce[A, B](values: List[A], mapper: A => B, reducer: List[B] => B): Par[B] =
      (values parMap mapper) map reducer
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    Par.fold(ints, 0, _ + _)

  def max(ints: IndexedSeq[Int]): Par[Int] =
    Par.fold(ints, Int.MinValue, _ max _)

  def countWords(str: List[String]): Par[Int] = {
    Par.mapReduce[String, Int](str, _.split("\\s+").length, _.sum)
  }

  val pool = Executors.newFixedThreadPool(10)
  try {
    val res = sum(Vector(1, 2, 3, 4)) runIn pool get()
    println(res)

    val range = Range(1, 1000, 97)
    val maxInt = max(range) runIn pool get()
    println(maxInt)

    val result = countWords(List("1", "2 2", "3 3   3")) runIn pool get()
    println(s"words count = $result")

  } finally {
    pool.shutdown()
  }

}
