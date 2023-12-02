package days

import utils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day17 {
  def main(args: Array[String]): Unit = {
    part2()
  }

  private def part2(): Unit = {
    val directions = readDirections()

    def nextDirection(int: Long): Direction = directions((int % directions.length).toInt)

    val scene = Scene()

    val rocks = 1000000000000L
    //val rocks = 2022L
    val cache = mutable.HashMap.empty[(ArrayBuffer[ArrayBuffer[Boolean]], Long), (Long, Long)]
    var rockCounter = rocks
    var windCounter = 0L
    while (rockCounter > 0) {
      if (rockCounter % 100 == 0)
        println(s"rock: ${rocks - rockCounter + 1}")
      if (cache.contains((scene.arr, windCounter % directions.length))) {
        val cachedScene = cache(scene.arr -> windCounter % directions.length)
        val cycleLength = (cachedScene._1 - rockCounter)
        if (cycleLength <= rockCounter) {
          println("HIT")
          val numberOfCyclesToJump = rockCounter / cycleLength
          val heightDiff = scene.highestPoint - cachedScene._2
          scene.yOffset = scene.yOffset + (heightDiff * numberOfCyclesToJump)
          rockCounter = rockCounter - (cycleLength * numberOfCyclesToJump)
        }
      }
      {
        cache.put(scene.arr -> windCounter % directions.length, (rockCounter, scene.highestPoint))
        scene.addNew(nextShape(rocks - rockCounter))
        //scene.show()
        var movementCounter = 0
        while (!scene.placedCurrent) {
          movementCounter = movementCounter + 1
          val windMovement = nextDirection(windCounter)
          windCounter = windCounter + 1
          scene.run(windMovement)
          //println(s"move: $movementCounter $windMovement")
          //scene.show()

          movementCounter = movementCounter + 1
          scene.run(Down)
          //println(s"move: $movementCounter")
          //scene.show()
        }
        //println(s"rock ${rocks - rockCounter + 1} placed")
        rockCounter = rockCounter - 1
      }
    }
    //scene.show()
    println(scene.highestPoint)
  }


  private def part1(): Unit = {
    val directions = readDirections()

    def nextDirection(int: Int): Direction = directions(int % directions.length)

    val scene = Scene()

    val rocks = 2022
    var rockCounter = rocks
    var windCounter = 0
    while (rockCounter > 0) {
      println(s"rock: ${rocks - rockCounter + 1}")
      scene.addNew(nextShape(rocks - rockCounter))
      //scene.show()
      var movementCounter = 0
      while (!scene.placedCurrent) {
        movementCounter = movementCounter + 1
        val windMovement = nextDirection(windCounter)
        windCounter = windCounter + 1
        scene.run(windMovement)
        //println(s"move: $movementCounter $windMovement")
        //scene.show()

        movementCounter = movementCounter + 1
        scene.run(Down)
        //println(s"move: $movementCounter")
        //scene.show()
      }
      //println(s"rock ${rocks - rockCounter + 1} placed")
      rockCounter = rockCounter - 1
    }
    //scene.show()
    println(scene.highestPoint)
  }

  private def readDirections(): Seq[Direction] = {
    readLines("day17.txt")
      .head
      .split("")
      .map(Direction.of)
  }

  case class Scene() {
    private val wide = 7
    private val yMaxValue = 100
    private val yChange = yMaxValue / 10
    var yOffset: Long = 0L
    var currShape: Shape = null
    val arr: ArrayBuffer[ArrayBuffer[Boolean]] = ArrayBuffer.fill(wide)(ArrayBuffer.fill(3)(false))
    var placedCurrent: Boolean = false

    def highestPoint: Long = {
      val r = arr.map { ys =>
        ys.zipWithIndex.filter(_._1).maxByOption(_._2)
          .map(_._2 + 1) // count in 0 index
          .getOrElse(0)
      }.maxOption.getOrElse(0) + yOffset
      //println(s"MAX: $r")
      r
    }

    def run(direction: Direction): Unit = {
      def cannotMoveThere(shape: Shape): Boolean = {
        val maxX = shape.points.maxBy(_.x).x
        val maxY = shape.points.maxBy(_.y).y
        if (maxX >= wide || maxY == -1) {
          return true
        }
        shape.points.map(p => arr(p.x)(p.y)).fold(false)(_ || _)
      }

      if (!placedCurrent && currShape != null) {
        val nextShape = currShape.move(direction.movement)
        if (nextShape.rightBorder < wide && nextShape.leftBorder >= 0) {
          if (cannotMoveThere(nextShape)) {
            if (direction == Down) {
              placedCurrent = true
              currShape.points.foreach(p => arr(p.x)(p.y) = true)
              currShape = null
              //println(s"placed after $direction")
            }
            else {
              //println(direction)
            }
          } else {
            //println(s"cannot move: $direction")
            currShape = nextShape
          }
        }
      }
    }

    def addNew(shape: Shape): Unit = {
      placedCurrent = false
      if (arr(0).length > yMaxValue) {
        //println(s"changed offset by $yChange from ${arr(0).length} to ${arr(0).length - yChange}, next offset ${yChange + yOffset}")
        arr.foreach(ys =>
          ys.remove(0, yChange)
        )
        yOffset = yOffset + yChange
      }
      var toAdd = ((highestPoint + shape.height + 3) - (arr(0).length + yOffset))
      if (toAdd < 0) toAdd = 0
      arr.foreach(
        ys => ys addAll ArrayBuffer.fill(toAdd.toInt)(false)
      )
      currShape = shape.move(2, (highestPoint - yOffset).toInt + 3)
    }

    def show(): Unit = {
      for (j <- Range.inclusive(arr(0).length - 1, 0, -1)) {
        print("|")
        for (i <- arr.indices) {
          val str = if (currShape != null && currShape.contains(i, j)) {
            "@"
          } else if (arr(i)(j)) {
            "#"
          } else {
            "."
          }
          print(str)
        }
        println("|")
      }
      println("+-------+")
      println()
    }
  }

  case class Shape(points: Point*) {
    def contains(x: Int, y: Int): Boolean = points.contains(Point(x, y))

    def move(p: Point): Shape = move(p.x, p.y)

    def move(x: Int, y: Int): Shape = {
      Shape(points.map(p => Point(p.x + x, p.y + y)): _*)
    }

    def rightBorder: Int = points.maxBy(_.x).x

    def leftBorder: Int = points.minBy(_.x).x

    def bottomBorder: Int = points.minBy(_.y).y

    def topBorder: Int = points.maxBy(_.y).y

    def height: Int = topBorder - bottomBorder + 1
  }

  val shapes = Seq(
    Shape(Point(0, 0), Point(1, 0), Point(2, 0), Point(3, 0)), // ####
    Shape(Point(1, 0), Point(0, 1), Point(1, 1), Point(2, 1), Point(1, 2)), // +
    Shape(Point(0, 0), Point(1, 0), Point(2, 0), Point(2, 1), Point(2, 2)), // l
    Shape(Point(0, 0), Point(0, 1), Point(0, 2), Point(0, 3)), // |
    Shape(Point(0, 0), Point(0, 1), Point(1, 0), Point(1, 1)) // #
  )

  def nextShape(int: Long): Shape = shapes((int % shapes.length).toInt)

  trait Direction {
    def movement: Point
  }

  object Direction {
    def of(s: String): Direction = s match {
      case "<" => Left
      case ">" => Right
    }
  }

  case object Left extends Direction {
    override def movement: Point = Point(-1, 0)
  }

  case object Right extends Direction {
    override def movement: Point = Point(1, 0)
  }

  case object Down extends Direction {
    override def movement: Point = Point(0, -1)
  }
}
