package log

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.util.matching.Regex

case class Request(thread: String, user: String, ip: String, action: Action, timestamp: LocalDateTime)

object Request {
  private val LogLine: Regex = """(http-[bn]io-\d+-exec-\d+)#.*\s+(\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}:\d{2}\.\d{3})\s\[.*\s(.*)\s(.*)\]\s.*\):?\s(.*)""".r
  private val DoAction: Regex = """.*doaction=(\w+).*""".r
  private val Type: Regex = """.*type=(\w+).*""".r
  private val DateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")

  def apply(logLine: String): Option[Request] = {
    logLine match {
      case LogLine(thread, timestamp, user, ip, text) =>
        for {
          action <- parseAction(text)
        } yield Request(thread, user, ip, action, parseTime(timestamp))
      case _ => None
    }
  }

  private def parseAction(text: String) = {
    text match {
      case DoAction("Go") =>
        text match {
          case Type(goType) => Some(GoAction(goType))
          case _ => Some(GoAction("Unknown"))
        }
      case DoAction(name) => Some(RegularAction(name))
      case _ => None
    }
  }

  private def parseTime(timeString: String) = {
    LocalDateTime.from(DateFormat.parse(timeString))
  }
}

