package func

import func.State.unit

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

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def flatMap[B](g: A => State[S, B]): State[S, B] = State { s =>
    val (a, s2) = run(s)
    g(a) run s2
  }

}

object State {
  def unit[S, A](a: A): State[S, A] = State { s => (a, s) }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- ra
    b <- rb
  } yield f(a, b)

  def map3[S, A, B, C, D](ra: State[S, A], rb: State[S, B], rc: State[S, C])(f: (A, B, C) => D): State[S, D] = for {
    a <- ra
    b <- rb
    c <- rc
  } yield f(a, b, c)

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](Nil))((f: State[S, A], acc: State[S, List[A]]) => map2(f, acc)(_ :: _))
}