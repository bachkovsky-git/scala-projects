import java.nio.file.Paths.get

object Main extends App {
  args match {
    case Array(first, second) => println(s"$first, $second")
    case _ => println("There should be two arguments")
  }

  (1 until 10).foreach(print)

  println()

  sealed trait FileType
  case object Directory extends FileType
  case object File extends FileType

  val javaHome = sys.env("JAVA_HOME")
  get(javaHome).toFile.listFiles
    .groupBy(file => if (file.isFile) File else Directory)
    .foreach { case (fileType, files) => println(fileType); files.foreach(println) }
}
