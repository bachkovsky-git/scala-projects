package func.typeclass.show

trait MyShow[A] { self =>
  def show(a: A): String

  def contramap[B](f: B => A) : MyShow[B] = b =>
    self.show(f(b))
}

object MyShow {
  def apply[A: MyShow]: MyShow[A] = implicitly[MyShow[A]]
}