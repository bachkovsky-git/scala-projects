package func.typeclass.show
import cats.Show
import cats.Show.show
import cats.syntax.show._

object CatsShowTest extends App {
  final case class Cat(name: String, age: Int, color: String)

  implicit val showCat: Show[Cat] =
    show(a => s"${a.name} is a ${a.age} year-old ${a.color} cat.")

  println(Cat("Giselle", 2, "gray") show)
}
