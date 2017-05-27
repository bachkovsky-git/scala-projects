import func.{Random, SimpleRandom}

val state = SimpleRandom(1)
assert(state.nextInt._1 == state.nextInt._1)
assert(state.nextInt._2.nextInt._1 == state.nextInt._2.nextInt._1)
assert(state.nextInt._1 != state.nextInt._2.nextInt._1)

val (l, r) = Random.ints(10).run(state)
assert(l.size == 10)
assert(l.toSet.size == 10)
assert(!l.toSet.contains(r.nextInt))


val rnd = for {
  d <- Random.double
  i <- Random.int
  p1 <- Random.nonNegativeInt
  p2 <- Random.nonNegativeInt
} yield (d, i, p1, p2)

val tuple: (Double, Int, Int, Int) = rnd.eval(state)

