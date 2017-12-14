package log

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import log.Action._

import scala.util.Try
import scala.util.matching.Regex

case class Request(thread: String, user: String, ip: String, action: Action, timestamp: LocalDateTime, text: String)

object Request {
  private val LogLine: Regex = """(.*?)#.*\s+(\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}:\d{2}\.\d{3})\s\[.*\s(.*)\s(.*)\]\s.*?\):?\s(.*)""".r
  private val DoAction: Regex = """.*doaction=(\w+).*""".r
  private val Type: Regex = """.*type=(\w+).*""".r
  private val DateFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")

  private val BeginTransaction: Regex = "(\\d+) begin!!!.*".r
  private val Transaction: Regex = ".*SQL\\((\\d+)\\).*".r
  private val EndTransaction: Regex = "(\\d+) end!!!.*".r

  def tryParse(logLine: String): Option[Request] = {
    logLine match {
      case LogLine(thread, timestamp, user, ip, text) =>
        for {
          time <- parseTime(timestamp)
          action <- parseAction(text)
        } yield Request(thread, user, ip, action, time, text)
      case _ => None
    }
  }

  private def parseAction(text: String) = {
    text match {
      case DoAction("Go")         =>
        text match {
          case Type(goType) => Some(GoAction(goType))
          case _            => Some(GoAction("Unknown"))
        }
      case DoAction(name)         => Some(RegularAction(name))

      case BeginTransaction(trId) => Some(TransactionStart(trId))
      case Transaction(trId)      => Some(TransactionAction(trId))
      case EndTransaction(trId)   => Some(TransactionEnd(trId))

      case _                      => None
    }
  }

  private def parseTime(timeString: String) = {
    Try(LocalDateTime.from(DateFormat.parse(timeString)))
      .toOption
      .orElse(Some(LocalDateTime.MIN))
  }
}
