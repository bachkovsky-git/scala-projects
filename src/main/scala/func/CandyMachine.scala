package func

import func.State.{get, modify, sequence}


object CandyMachine extends App {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Candy {
    def update: Input => Machine => Machine = (i: Input) => (machine: Machine) =>
      (i, machine) match {
        case (_, Machine(_, 0, _))               => machine
        case (Coin, Machine(false, _, _))        => machine
        case (Turn, Machine(true, _, _))         => machine
        case (Coin, Machine(true, candy, coin))  => Machine(locked = false, candy, coin + 1)
        case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
      }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
      _ <- sequence(inputs map (update andThen modify[Machine]))
      s <- get
    } yield (s.coins, s.candies)
  }


  val simulateMachine = Candy.simulateMachine(List(Coin, Turn, Coin, Turn, Turn, Turn))
  private val res = simulateMachine.exec(Machine(locked = true, candies = 2, coins = 0))
  println(s"res = $res")
}
