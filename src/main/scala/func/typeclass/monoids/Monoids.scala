package func.typeclass.monoids
import cats.Monoid
import cats.syntax.monoid._
import cats.syntax.option._
import cats.instances.string._
import cats.instances.int._
import cats.instances.option._

object Monoids extends App {
  private val s: String = "Hi" |+| " there" |+| ", " |+| "dude!"

  def add[A: Monoid](items: List[A]): A = items.fold(Monoid[A].empty)(_ |+| _)

  assert {
    add(List(1.some, 2.some, 3.some)).contains(add(List(1, 2, 3)))
  }

  assert {
    add(List(1.some, None, 2.some, None, 3.some)).contains(add(List(1, 2, 3)))
  }

  case class Order(totalCost: Double, quantity: Double)

  implicit object MOrder extends Monoid[Order] {
    override val empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  assert {
    add(List(Order(1, 1), Order(2, 2), Order(3,3))) == Order(6, 6)
  }
}
