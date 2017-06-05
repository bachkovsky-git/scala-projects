package func.typeclass.show

object MyShowSyntax {
  implicit class PrintOps[A](a: A) {
    def show(implicit instance: MyShow[A]): String =
      instance.show(a)

    def print(implicit instance: MyShow[A]): Unit =
      println(a show)
  }
}
