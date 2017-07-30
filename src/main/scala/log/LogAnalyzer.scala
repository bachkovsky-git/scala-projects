package log

import java.nio.file.Paths.get

import scala.io.Source

object LogAnalyzer extends App {
  args match {
    case Array(path) => analyzePath(path)
    case _ => println("Specify log directory")
  }

  def analyzePath(logDir: String): Unit = {
    val start = System.currentTimeMillis

    val requests = parseLogs(logDir)

    //todo add some analyze
    requests.foreach(println)

    println(s"duration = ${System.currentTimeMillis - start}ms")
  }

  private def parseLogs(logDir: String) = {
    val requests = for {
      logFile <- get(logDir).toFile.listFiles.view
      if logFile.getName.startsWith("SystemOut")
      logLine <- Source.fromFile(logFile).getLines
      if logLine.length < 5000 && logLine.contains("doaction=")
    } yield Request(logLine)

    requests.force.flatten
  }
}
