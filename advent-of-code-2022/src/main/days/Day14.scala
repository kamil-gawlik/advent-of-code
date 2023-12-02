package days

import utils._


object Day14 {
  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

  private def part1(): Unit = {
    val scene = setupScene()
    var counter = 0
    while (dropSand(scene.arr, scene.sandSource)._1) {
      counter = counter + 1
      //scene.show()
    }
    scene.show()
    println(counter)
  }

  private def part2(): Unit = {
    val scene = setupScene(2, 1000, 1000)
    var counter = 1
    while ( {
      val (success, lastPosition) = dropSand(scene.arr, scene.sandSource);
      success && lastPosition != scene.sandSource
    }) {
      counter = counter + 1
      //scene.show()
    }
    scene.show()
    println(counter)
  }

  // returns true if found position for sand
  private def dropSand(arr: Array[Array[Elem]], sandPosition: Point): (Boolean, Point) = {
    if (arr.escapes(sandPosition)) {
      return (false, null)
    }
    if (!arr.at(sandPosition.down).isSolid) {
      dropSand(arr, sandPosition.down)
    }
    else {
      if (arr.escapes(sandPosition.leftDown) || arr.escapes(sandPosition.rightDown)) {
        return (false, null)
      }
      (arr.at(sandPosition.leftDown).isSolid, arr.at(sandPosition.rightDown).isSolid) match {
        case (false, _) => dropSand(arr, sandPosition.leftDown)
        case (true, false) => dropSand(arr, sandPosition.rightDown)
        case (true, true) => {
          arr(sandPosition.x)(sandPosition.y) = Sand
          return (true, sandPosition)
        }
      }
    }
  }

  private def setupScene(yOffset: Int = 0, leftXOffset: Int = 0, rightXOffset: Int = 0): Scene = {
    val rockCoordinates = readLines("day14.txt")
      .map(_.split(" -> ").map(xy => {
        val tmp = xy.split(",")
        (tmp(0).toInt, tmp(1).toInt)
      }))
    val min_x = rockCoordinates.flatten.flatMap(x => Array(x._1)).min - leftXOffset
    val max_x = rockCoordinates.flatten.flatMap(x => Array(x._1)).max + rightXOffset
    val max_y = rockCoordinates.flatten.flatMap(x => Array(x._2)).max + yOffset
    val min_y = rockCoordinates.flatten.flatMap(x => Array(x._2)).min

    val arr: Array[Array[Elem]] = Array.fill(max_x - min_x + 1)(Array.fill(max_y + 1)(Air))
    rockCoordinates.foreach { rock =>
      rock.sliding(2).foreach { line =>
        val from = line(0)
        val to = line(1)

        def step(from: Int, to: Int): Int = {
          val diff = to - from
          if (diff == 0) {
            1
          } else
            (diff / Math.abs(diff))
        }

        for (i <- from._1 to to._1 by (step(from._1, to._1))) {
          for (j <- from._2 to to._2 by step(from._2, to._2)) {
            //println(s"${i - min_x} ${j}")
            arr(i - min_x)(j) = Rock
          }
        }
      }
    }
    if (yOffset != 0) {
      for (i <- arr.indices) {
        arr(i)(arr(0).length - 1) = Rock
      }
    }
    val sandSource = Point(500 - min_x, 0)
    arr(sandSource.x)(sandSource.y) = SandSource
    Scene(arr, sandSource)
  }

  case class Scene(arr: Array[Array[Elem]], sandSource: Point) {
    def show(): Unit = {
      for (i <- arr(0).indices) {
        for (j <- arr.indices) {
          print(arr(j)(i).toString)
        }
        println()
      }
      println()
    }
  }

  case class Point(x: Int, y: Int) {
    def down: Point = Point(x, y + 1)

    def leftDown: Point = Point(x - 1, y + 1)

    def rightDown: Point = Point(x + 1, y + 1)
  }

  implicit class ArrOps[T](arr: Array[Array[T]]) {
    def at(p: Point): T = arr(p.x)(p.y)

    def escapes(p: Point): Boolean = {
      (p.x < 0 || p.y < 0 || p.x >= arr.length || p.y >= arr(0).length)
    }
  }

  trait Elem {
    def toString: String

    def isSolid: Boolean
  }

  case object Rock extends Elem {
    override def toString: String = "#"

    override def isSolid: Boolean = true
  }

  case object Sand extends Elem {
    override def toString: String = "0"

    override def isSolid: Boolean = true
  }

  case object Air extends Elem {
    override def toString: String = "."

    override def isSolid: Boolean = false
  }

  case object SandSource extends Elem {
    override def toString: String = "+"

    override def isSolid: Boolean = true
  }
}
