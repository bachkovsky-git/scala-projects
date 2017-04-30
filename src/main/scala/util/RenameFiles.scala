package util

import java.nio.file.Paths


object RenameFiles extends App {
  args match {
    case Array(path) => renameIn(path)
  }

  def renameIn(path: String) {
   val flacFiles = Paths.get(path).toFile.listFiles((_, name) => name.endsWith("flac"))

    //a1.watching.the.river.flow.flac -> Watching The River Flow.flac
    for {
      file <- flacFiles
      nameParts = file . getName . split ("\\.") . tail
      newName = nameParts . init . map { _.capitalize } . mkString(" ") + "." + nameParts.last
      newFile = Paths.get(path, newName).toFile
    } { file.renameTo(newFile) }
  }
}
