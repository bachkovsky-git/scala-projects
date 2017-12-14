package akka

import akka.Messages.{Exit, Finished, Response}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import shapes._

object Messages {

  object Exit

  object Finished

  case class Response(message: String)

}

class ShapesDrawingActor extends Actor {
  override def receive = {
    case s: Shape   =>
      s.draw(str => println(s"ShapesDrawingActor: $str"))
      sender ! Response(s"ShapesDrawingActor: $s drawn")
    case Exit       =>
      println(s"ShapesDrawingActor: exiting...")
      sender ! Finished
    case unexpected =>
      val response = Response(s"ERROR: Unknown message: $unexpected")
      println(s"ShapesDrawingActor: $response")
      sender ! response
  }
}

private object Start

object ShapesDrawingDriver {
  def main(args: Array[String]) {
    val system = ActorSystem("DrawingActorSystem", ConfigFactory.load())
    val drawer = system.actorOf(Props(new ShapesDrawingActor), "drawingActor")
    val driver = system.actorOf(Props(new ShapesDrawingDriver(drawer)), "drawingService")
    driver ! Start
  }
}

class ShapesDrawingDriver(drawerActor: ActorRef) extends Actor {
  override def receive = {
    case Start              =>
      drawerActor ! Circle(Point(0.0, 0.0), 1.0)
      drawerActor ! Rectangle(Point(0.0, 0.0), 2, 5)
      drawerActor ! 3.14159
      drawerActor ! Triangle(Point(0.0, 0.0), Point(2.0, 0.0), Point(1.0, 2.0))
      drawerActor ! Exit
    case Finished           =>
      println(s"ShapesDrawingDriver: cleaning up...")
      context.system.terminate()
    case response: Response =>
      println("ShapesDrawingDriver: Response = " + response)
    case unexpected         =>
      println("ShapesDrawingDriver: ERROR: Received an unexpected message = "
        + unexpected)
  }
}