import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Paths.get

object Main extends App {
  args match {
    case Array(first, second) => println(s"$first, $second")
    case _ => println("There should be two arguments")
  }

  (1 until 10).foreach(print)

  println()

  sealed trait FileType

  object FileType {
    def apply(file: File): FileType = if (file.isFile) File else Directory
  }
  case object Directory extends FileType
  case object File extends FileType

  val javaHome = sys.env("JAVA_HOME")
  get(javaHome).toFile.listFiles
    .groupBy(FileType(_))
    .foreach { case (fileType, files) => println(fileType); files.foreach(println) }

  def safe[A <: AutoCloseable](resource: A)(op: A => Unit): Unit = {
    try {
      op(resource)
    } finally {
      resource.close()
    }
  }

  val home = sys.env("HOME")
  private val hash = home ##
  val testFile = get(home, "testFile.txt").toFile

  safe(new PrintWriter(testFile)) {
    _.println("kekeke")
  }

  safe(new FileWriter(testFile, true)) {
    _.append("asd")
  }
}
