package func.typeclass.show

trait MyShow[A] {
  def show(a: A): String
}