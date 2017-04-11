package log

import java.nio.file.Paths

import scala.io.Source
import scala.language.postfixOps

object LogAnalyzer extends App {
  val ActionParameter = "doaction="

  args match {
    case Array(path) => analyzePath(path)
    case _ => println("Specify log directory")
  }

  def analyzePath(path: String): Unit = {
    val start = System.currentTimeMillis

    val files = Paths.get(path).toFile.listFiles

    val requests = files
                    .view
                    .filter(_.getName.startsWith("SystemOut"))
                    .map(Source.fromFile)
                    .flatMap(_.getLines)
                    .filter(_.length < 5000)
                    .filter(_.contains(ActionParameter))
                    .flatMap(Request.parseSystemLog)
                    .toList


/*    val histByTime = new HashMap[LocalDateTime, Set[Request]] with MultiMap[LocalDateTime, Request]
    for (elem <- requests) {
      histByTime.addBinding(elem.timestamp.truncatedTo(ChronoUnit.MINUTES), elem)
    }

    val byAction = histByTime(LocalDateTime.of(2017, 4, 6, 15, 0,0)).toSeq.groupBy(_.action).toSeq.sortBy(_._2.size).reverse
    val sortedByTime = histByTime.toSeq.sortWith((first, second) => first._1.compareTo(second._1) < 0)
    val requestCountByMinute = sortedByTime.map(t => (t._1, t._2.size))
*/
    println(s"duration = ${System.currentTimeMillis - start}ms")
  }
}
