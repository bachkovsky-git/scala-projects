package func

case class State[S, +A](run: S => (A, S)) {
  /**
    * Evaluate a state computation with the given initial state and return the final value, discarding the final state.
    *
    * @param s initial value
    * @return value of the state computation
    *
    */
  def eval(s: S): A = run(s)._1

  /**
    * Evaluate a state computation with the given initial state and return the final state, discarding the final value.
    *
    * @param s initial value
    * @return final state
    */
  def exec(s: S): S = run(s)._2


  def map[B](f: A => B): State[S, B] = for {
    a <- this
  } yield f(a)

  def flatMap[B](g: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    g(a) run s2
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State { s => (a, s) }

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    for {
      a <- ra
      b <- rb
    } yield f(a, b)
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State { initState =>
    var list = List.empty[A]
    var currentState = initState

    for (f <- fs) {
      val (a, nextState) = f.run(currentState)
      list = list :+ a
      currentState = nextState
    }

    (list, currentState)
  }
}