package log

import java.nio.file.Paths.get
import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import log.Action.{TransactionAction, TransactionEnd, TransactionStart}

import scala.collection.SeqView
import scala.io.Source

object LogAnalyzer extends App {
  args match {
    case Array(path) => analyzePath(path)
    case _ => println("Specify log directory")
  }

  def analyzePath(logDir: String): Unit = {
    val start = System.currentTimeMillis

    val transactionsOnly: (Request) => Boolean = req => req.action match {
      case _: TransactionStart | _: TransactionAction | _: TransactionEnd => true
      case _                                                              => false
    }

    implicit val dateOrder: Ordering[LocalDateTime]  = (x: LocalDateTime, y: LocalDateTime) => x.compareTo(y)

    val requests = parseLogs(logDir)
                   .flatten
                   .filter(transactionsOnly)
                   .groupBy(_.action.name)
                   .filter { case (_, trs) => trs.exists(_.text.contains("SELECT 1 FROM")) }
                   .mapValues(trs => {
                     val forced = trs.force
                     val start = forced.head.timestamp
                     val end = forced.last.timestamp
                     (start, end, start.until(end, ChronoUnit.MILLIS))
                   })
//                   .filter { case (_, (_, _, dur)) => dur > 2000 }
                   .toList
                   .sortBy(_._2._1)

    println("|| transactionid || start || end || duration (ms) ||")
    requests.foreach(r => println(s"|${r._1}|${r._2._1}|${r._2._2}|${r._2._3}|"))
    //todo add some analyze
    //    requests.foreach(println)

    println(s"duration = ${System.currentTimeMillis - start}ms")
  }

  private def parseLogs(logDir: String): SeqView[Option[Request], Array[Option[Request]]] = for {
    logFile <- get(logDir).toFile.listFiles.view
    if logFile.getName.startsWith("SystemOut")
    logLine <- Source.fromFile(logFile).getLines.toTraversable.par
    if logLine.length < 5000
  } yield Request(logLine)

}
