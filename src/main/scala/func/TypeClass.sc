import java.util.Date

import scala.language.postfixOps
import scala.math.Ordering

//implicit val min = Ordering.fromLessThan[Int](_ < _)
implicit val max = Ordering.fromLessThan[Int](_ > _)

List(3, 4, 2) sorted

/**
  * type class
  */
trait HtmlRenderer[A] {
  def render(a: A): String
}

final case class Person(name: String, email: String)

/**
  * type class instances
  */
implicit object PersonHtmlRenderer extends HtmlRenderer[Person] {
  override def render(a: Person): String = s"<span>${a.name} &lt;${a.email}&gt;</span>"
}

implicit object DateHtmlRenderer extends HtmlRenderer[Date] {
  override def render(date: Date): String = s"<span>${date.toString}</span>"
}

trait Equal[A] {
  def equal(a1: A, a2: A): Boolean
}

object Eq {
  def apply[A](implicit inst: Equal[A]): Equal[A] = inst
}

implicit object PersonEmailEqual extends Equal[Person] {
  override def equal(a1: Person, a2: Person): Boolean = a1.email == a2.email
}

object PersonEqual extends Equal[Person] {
  override def equal(a1: Person, a2: Person): Boolean = a1 == a2
}

object HtmlUtil {
  def apply[A](implicit writer: HtmlRenderer[A]): HtmlRenderer[A] = {
    writer
  }
}

// select type class instance by type
// bad: code duplication
HtmlUtil[Person].render(Person("John", "john@example.com"))
HtmlUtil[Date].render(new Date())

val eq = Eq[Person].equal(Person("Ali", "ali@abab.ua"), Person("Abu", "ali@abab.ua"))
assert(eq)

implicit class HtmlOps[T](data: T) {
  //instance type can be infered here
  def render(implicit instance: HtmlRenderer[T]): String =
    instance.render(data)
}

//look ma, without code duplication
val rendered = Person("ses", "ses@kek.com") render
val dateRendered = new Date() render

implicit class EqOps[T](o1: T) {
  def equal(o2: T)(implicit instance: Equal[T]): Boolean =
    instance.equal(o1, o2)
}

val theyAreEqual = Person("Ali", "ali@abab.ua") equal Person("Abu", "ali@abab.ua")

// context bound: expands to generic type parameter
// and implicit parameter for context
def pageTemplate[A : HtmlRenderer](body: A) : String = {
  s"<html><head>...</head><body>${body render}</body></html>"
}

val template = pageTemplate(Person("ses", "ses@kek.com"))
