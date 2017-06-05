package func

object Streams {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) map { case (a, s) => a #:: unfold(s)(f) } getOrElse Stream.empty
}

