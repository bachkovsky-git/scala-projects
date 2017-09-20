package log

sealed trait Action {
  def name: String
}

object Action {
  case class RegularAction(name: String) extends Action

  case class GoAction(goType: String) extends Action {
    override def name: String = "Go: " + goType
  }

  case class TransactionStart(name: String) extends Action
  case class TransactionAction(name: String) extends Action
  case class TransactionEnd(name: String) extends Action
}

