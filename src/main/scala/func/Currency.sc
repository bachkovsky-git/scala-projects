println()

abstract class CurrencyZone {
  type Currency <: AbstractCurrency

  def make(x: Long): Currency

  abstract class AbstractCurrency {
    val amount: Long

    def designation: String

    override def toString: String = amount + " " + designation

    def +(that: Currency): Currency =
      make(this.amount + that.amount)

    def *(x: Double): Currency =
      make((this.amount * x).toLong)
  }
}

object US extends CurrencyZone {

  case class Dollar(amount: Long) extends AbstractCurrency {
    override val designation = "USD"
  }

  type Currency = Dollar

  def make(x: Long) = Dollar(x)
}
