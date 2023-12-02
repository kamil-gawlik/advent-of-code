import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

package object utils {

  def readLines(fileName: String): Array[String] = {
    val source = Source.fromResource(fileName)
    try source.getLines().toArray finally source.close()
  }

  def readLinesSplitEmptyLine(fileName: String): Array[Array[String]] = {
    val source = Source.fromResource(fileName)
    try source.mkString
      .split("\n\n")
      .map(_.split("\n")) finally source.close()
  }

  case class Point(x: Int, y: Int) {
    def move(v: Vect): Point = Point(x + v.x, y + v.y)
  }

  implicit class PointOps(p: Point) {
    def align(x: Int, y: Int): Point = Point((p.x + x) % x, (p.y + y) % y)

    def isCorrect(x: Int, y: Int): Boolean = (p.x >= 0 && p.y >= 0 && p.x < x && p.y < y)
  }

  implicit class ArrOps[T](arr: ArrayBuffer[ArrayBuffer[T]]) {
    def at(point: Point): T = arr(point.x)(point.y)
  }

  case class Vect(x: Int, y: Int)

  object Vect {
    val UP = Vect(-1, 0)
    val DOWN = Vect(1, 0)
    val LEFT = Vect(0, -1)
    val RIGHT = Vect(0, 1)
  }

  def floodFill[Position](start: Position, getNeighbors: Position => Set[Position]): Set[Position] = {
    @tailrec
    def helper(visited: Set[Position], open: Queue[Position]): Set[Position] = {
      open.dequeueOption match {
        case Some((current, open)) =>
          val neighbors = getNeighbors(current) -- visited - current
          val newVisited = visited ++ neighbors
          val newOpen = open.enqueueAll(neighbors)
          helper(newVisited, newOpen)
        case None =>
          visited
      }
    }

    helper(Set(start), Queue(start))
  }

  def timeNs[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println("elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def timeMs[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println("elapsed time: " + (t1 - t0) + "ms")
    result
  }

  def timeMs[R](marker: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println(s"[$marker] elapsed time: " + (t1 - t0) + "ms")
    result
  }

}
