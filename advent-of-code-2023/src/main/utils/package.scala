import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

package object utils {

  type M[T] = Seq[Seq[T]]
  
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

  case class Point(x: Int, y: Int) {
    def move(v: Vect): Point = Point(x + v.x, y + v.y)

    def neighbors: Seq[Point] = Seq(Vect.UP, Vect.DOWN, Vect.LEFT, Vect.RIGHT).map(this.move)

    def neighborsAdjacent: Seq[Point] = Vect.allAdjacent.map(this.move)

    // for now works only for right movement
    def allWithin(v: Vect): Seq[Point] = (0 until v.y).map { yy => this.move(Vect(v.x, yy)) }

  }

  extension (p: Point) {
    def align(x: Int, y: Int): Point = Point((p.x + x) % x, (p.y + y) % y)

    def isCorrect(x: Int, y: Int): Boolean = (p.x >= 0 && p.y >= 0 && p.x < x && p.y < y)

    def isCorrect[T](using arr: M[T]): Boolean = (p.x >= 0 && p.y >= 0 && p.x < arr.length && p.y < arr.head.length)
  }


  extension[T] (arr: Array[Array[T]]) {
    def at(point: Point): T = arr(point.x)(point.y)

    def show(title: String = ""): Unit = {
      println(title)
      for (i <- arr.indices) {
        for (j <- arr(0).indices) {
          print(arr(i)(j))
        }
        println()
      }
      println()
    }
  }

  extension[T] (arr: M[T]) {

    def at(point: Point): T = arr(point.x)(point.y)


    def allPoints: Seq[Point] = {
      var s = Seq.empty[Point]
      for (i <- arr.indices) {
        for (j <- arr(0).indices) {
          s = s :+ Point(i, j)
        }
      }
      s
    }

    def showM(title: String = ""): Unit = {
      println(title)
      for (i <- arr.indices) {
        for (j <- arr(0).indices) {
          print(arr(i)(j))
        }
        println()
      }
      println()
    }
  }

  case class Vect(x: Int, y: Int) {
    def scale(scale: Int) = Vect(x * scale, y * scale)
  }

  object Vect {
    val UP = Vect(-1, 0)
    val DOWN = Vect(1, 0)
    val LEFT = Vect(0, -1)
    val RIGHT = Vect(0, 1)

    val UPL = Vect(-1, -1)
    val UPR = Vect(-1, 1)
    val DOWNL = Vect(1, -1)
    val DOWNR = Vect(1, 1)

    val allAdjacent: Seq[Vect] = Seq(
      Vect(-1, -1), Vect(-1, 0), Vect(-1, 1),
      Vect(0, -1), Vect(0, 1),
      Vect(1, -1), Vect(1, 0), Vect(1, 1)
    )
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

  def timeNs[R](marker: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    println(s"[$marker] elapsed time: " + (t1 - t0) + "ns")
    result
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
