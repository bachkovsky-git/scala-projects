package log

import java.nio.file.Paths.get
import java.time.{Duration, LocalDateTime}

import log.Action.SqlTransaction

import scala.io.Source

object LogAnalyzer extends App {
  args match {
    case Array(path) => analyzePath(path)
    case _           => println("Specify log directory")
  }

  def analyzePath(logDir: String): Unit = {
    val start = System.currentTimeMillis

    implicit val dateOrder: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) => x.compareTo(y)

    case class Transaction(id: String, start: LocalDateTime, end: LocalDateTime) {
      val duration: Duration = Duration.between(start, end)
    }

    val sqlTransactions: (Request) => Boolean = req => req.action match {
      case _: SqlTransaction => true
      case _                 => false
    }

    val validationQueries: ((String, Traversable[Request])) => Boolean = {
      case (_, reqs) => reqs.size == 3 && reqs.exists(_.text.contains("SELECT 1 FROM"))
    }

    val requestsStream = parseLogs(logDir)

    val requests = requestsStream
                   .filter(sqlTransactions)
                   .groupBy(_.action.name)
                   .filter(validationQueries)
                   .map { case (id, trs) => Transaction(id, trs.head.timestamp, trs.last.timestamp) }
                   .toList
                   .sortBy(_.start)

    println("|| transactionid || start || end || duration (ms) ||")
    requests.foreach(r => println(s"|${r.id}|${r.start}|${r.end}|${r.duration.toMillis}|"))

    println(s"duration = ${System.currentTimeMillis - start}ms")
  }

  def parseLogs(logDir: String) = {
    val result = for {
      logFile <- get(logDir).toFile.listFiles.view
      if logFile.getName.startsWith("SystemOut")
      logLine <- Source.fromFile(logFile).getLines.toTraversable.par
      if logLine.length < 5000
    } yield Request.tryParse(logLine)

    result.flatten
  }

}
