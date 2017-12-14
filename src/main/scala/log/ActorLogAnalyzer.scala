package log

import java.io.File
import java.nio.file.Paths.get
import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.routing._
import com.typesafe.config.ConfigFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.io.Source
import scala.concurrent.duration._

object ActorLogAnalyzer extends App {
  args match {
    case Array(path) =>
      val system = ActorSystem("LogAnalyzingActorSystem", ConfigFactory.load())
      val entryActor = system.actorOf(SmallestMailboxPool(5).props(Props[LogEntryAnalyzerActor]))
      val fileActor = system.actorOf(SmallestMailboxPool(5).props(Props(new FileAnalyzingActor(entryActor))))
      val dirActor = system.actorOf(SmallestMailboxPool(5).props(Props(new DirectoryAnalyzingActor(fileActor))))

      dirActor ! LogPath(path)

//      Thread.sleep(TimeUnit.MINUTES.toMillis(2))
//      system.terminate()
      Await.result(system.whenTerminated, 2 minutes)
    case _           => println("Specify log directory")
  }
}

case class LogPath(path: String) extends AnyVal

case class LogLine(line: String) extends AnyVal

class DirectoryAnalyzingActor(fileAnalyzer: ActorRef) extends Actor {
  override def receive = {
    case LogPath(path) =>
      for {
        file <- get(path).toFile.listFiles
        if file.getName.startsWith("SystemOut")
      } {
        fileAnalyzer ! file
      }
  }
}

class FileAnalyzingActor(entryAnalyzer: ActorRef) extends Actor {
  override def receive = {
    case logFile: File =>

      val logEntry = new mutable.StringBuilder()
      for (line <- Source.fromFile(logFile).getLines()) {
        if (line.startsWith("http") || line.startsWith("Mira")) {
          if (logEntry.isEmpty) {
            logEntry.append(line)
          } else {
            entryAnalyzer ! LogLine(logEntry.toString())
            logEntry.clear()
          }
        } else {
          logEntry.append(" ").append(line)
        }
      }
  }
}

class LogEntryAnalyzerActor extends Actor {
  var list: mutable.ListBuffer[Request] = new ListBuffer[Request]

  override def receive = {
    case LogLine(line) =>
      Request.tryParse(line).foreach(println)
  }
}
