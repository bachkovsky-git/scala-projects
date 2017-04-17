package log

import java.nio.file.Paths.get

import scala.io.Source
import scala.language.postfixOps

object LogAnalyzer extends App {
  val ActionParameter = "doaction="

  args match {
    case Array(path) => analyzePath(path)
    case _ => println("Specify log directory")
  }

  def analyzePath(logDir: String): Unit = {
    val start = System.currentTimeMillis

    val requests = for {
      logFile <- get(logDir).toFile.listFiles
      if logFile.getName.startsWith("SystemOut")
      logLine <- Source.fromFile(logFile).getLines
      if logLine.length < 5000 && logLine.contains(ActionParameter)
    } yield Request(logLine)

    requests.foreach(println)

    println(s"duration = ${System.currentTimeMillis - start}ms")
  }
}
