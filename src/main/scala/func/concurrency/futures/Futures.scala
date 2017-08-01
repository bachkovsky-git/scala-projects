package func.concurrency.futures


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}


object Futures {
  def main(args: Array[String]): Unit = {
    val fut1 = Future { Thread.sleep(1000); 21 + 21 }
    val fut2 = Future { Thread.sleep(1000); 21 * 21 }

    val fut3 = for {
      x <- fut1
      y <- fut2
      s = 100 * y + x
      if s % 2 == 0
    } yield s

    fut3.onComplete {
      case Success(i)  => println(i)
      case Failure(ex) => ex.printStackTrace()
    }
    val res = Await.result(fut3, 10.seconds)

    //avoid dead code elimination
    consume(res)
  }

  private def consume(res: Int) = {
    if (res + 1 >= res) print("")
  }
}
