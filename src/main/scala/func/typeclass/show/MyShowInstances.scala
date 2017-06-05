package func.typeclass.show

import scala.language.postfixOps

/**
  * Some default implementations for Show
  */
object MyShowInstances {

  implicit object MyShowString extends MyShow[String] {
    override def show(a: String): String = a
  }

  implicit object MyShowInteger extends MyShow[Int] {
    override def show(a: Int): String = a toString
  }
}
