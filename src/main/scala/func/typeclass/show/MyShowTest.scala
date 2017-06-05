package func.typeclass.show

import func.typeclass.show.MyShowSyntax._

object MyShowTest extends App {

  final case class Cat(name: String, age: Int, color: String)

  implicit object MyShowCat extends MyShow[Cat] {
    override def show(a: Cat): String =
      s"${a.name} is a ${a.age} year-old ${a.color} cat."
  }

  Cat("Giselle", 2, "gray") print
}

