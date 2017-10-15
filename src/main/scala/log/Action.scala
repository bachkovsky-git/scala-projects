package log

sealed trait Action {
  def name: String
}

object Action {
  case class RegularAction(name: String) extends Action

  case class GoAction(goType: String) extends Action {
    override def name: String = "Go: " + goType
  }

  abstract class SqlTransaction extends Action {
    def id: String = name
  }

  case class TransactionStart(name: String) extends SqlTransaction
  case class TransactionAction(name: String) extends SqlTransaction
  case class TransactionEnd(name: String) extends SqlTransaction
}

