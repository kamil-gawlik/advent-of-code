package utils
import scala.io.Source

trait read {

  def readLines(fileName: String): Array[String] = {
    val source = Source.fromResource(fileName)
    try source.getLines().toArray finally source.close()
  }

  def readAsMatrix(fileName: String): M[Char] = {
    readLines(fileName).map(_.toCharArray.toSeq)
  }

  def readLinesSplitEmptyLine(fileName: String): Array[Array[String]] = {
    val source = Source.fromResource(fileName)
    try source.mkString
      .split("\n\n")
      .map(_.split("\n")) finally source.close()
  }

}
