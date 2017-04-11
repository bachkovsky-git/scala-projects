package log

sealed trait Action {
  def name: String
}

case class RegularAction(name: String) extends Action

case class GoAction(goType: String) extends Action {
  override def name: String = "Go: " + goType
}

