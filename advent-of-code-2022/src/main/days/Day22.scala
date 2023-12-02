package days

import days.Day22.Element.{Empty, Walkable, Wall}
import days.Day22.Rotation.{LeftRotation, RightRotation}
import utils._

import scala.collection.mutable.ArrayBuffer

object Day22 {
  def main(args: Array[String]): Unit = {
    part2()
  }

  private def part2(): Unit = {
    val (scene, commands) = readMap()
    traverseCube(scene, commands)
  }

  private def part1(): Unit = {
    val (scene, commands) = readMap()
    traverseMap(scene, commands)
  }

  def traverseCube(scene: Scene, commands: Seq[Command]): Unit = {
    var currentPoint: Walkable = scene.arr(0).find(_.isInstanceOf[Walkable]).get.asInstanceOf[Walkable]
    var currentDirection: Direction = Right

    def vectOf(direction: Direction): Vect = direction match {
      case Up => Vect.UP
      case Down => Vect.DOWN
      case Left => Vect.LEFT
      case Right => Vect.RIGHT
    }

    for (c <- commands) c match {
      case Rotate(rotation) => {
        currentDirection = currentDirection.rotate(rotation)
      }
      case Move(steps) =>
        (0 until steps).foreach { _ =>
          val (newPoint, newDirection) = {
            val cubeX = currentPoint.point.x / 50
            val cubeY = currentPoint.point.y / 50
            val modX = currentPoint.point.x % 50
            val modY = currentPoint.point.y % 50
            val nextPoint = currentPoint.point.move(vectOf(currentDirection))
            val nextX = if (nextPoint.x >= 0) nextPoint.x / 50 else nextPoint.x
            val nextY = if (nextPoint.y >= 0) nextPoint.y / 50 else nextPoint.y

            val (candidatePosition, candidateDirection) = if (cubeX == nextX && cubeY == nextY) {
              (nextPoint, currentDirection)
            } else {
              // Cube sides:
              //  AB
              //  C
              // ED
              // F
              (cubeY, cubeX, nextX, nextY) match {
                // A to F
                case (1, 0, -1, 1) => Point(150 + modX, 0) -> Right
                // A to C
                case (1, 0, 1, 1) => nextPoint -> currentDirection
                // A to E
                case (1, 0, 0, 0) => Point(149 - modY, 0) -> Right
                // A to B
                case (1, 0, 0, 2) => nextPoint -> currentDirection

                // B to F
                case (2, 0, -1, 2) => Point(199, 0 + modX) -> currentDirection
                // B to C
                case (2, 0, 1, 2) => Point(50 + modX, 99) -> Left
                // B to A
                case (2, 0, 0, 1) => nextPoint -> currentDirection
                // B to D
                case (2, 0, 0, 3) => Point(149 - modY, 99) -> Left

                // C to A
                case (1, 1, 0, 1) => nextPoint -> currentDirection
                // C to D
                case (1, 1, 2, 1) => nextPoint -> currentDirection
                // C to E
                case (1, 1, 1, 0) => Point(100, modY) -> Down
                // C to B
                case (1, 1, 1, 2) => Point(49, 100 + modY) -> Up

                // D to C
                case (1, 2, 1, 1) => nextPoint -> currentDirection
                // D to F
                case (1, 2, 3, 1) => Point(150 + modX, 49) -> Left
                // D to E
                case (1, 2, 2, 0) => nextPoint -> currentDirection
                // D to B
                case (1, 2, 2, 2) => Point(49 - modY, 149) -> Left

                // E to C
                case (0, 2, 1, 0) => Point(50 + modX, 50) -> Right
                // E to F
                case (0, 2, 3, 0) => nextPoint -> currentDirection
                // E to A
                case (0, 2, 2, -1) => Point(49 - modY, 50) -> Right
                // E to D
                case (0, 2, 2, 1) => nextPoint -> currentDirection

                // F to E
                case (0, 3, 2, 0) => nextPoint -> currentDirection
                // F to B
                case (0, 3, 4, 0) => Point(0, 100 + modX) -> currentDirection
                // F to A
                case (0, 3, 3, -1) => Point(0, 50 + modY) -> Down
                // F to D
                case (0, 3, 3, 1) => Point(149, 50 + modY) -> Up
              }
            }
            if (scene.arr.at(candidatePosition).isInstanceOf[Wall.type]) (currentPoint, currentDirection) else (scene.arr.at(candidatePosition).asInstanceOf[Walkable], candidateDirection)
          }
          currentPoint = newPoint
          currentDirection = newDirection
        }
    }
    println((currentPoint.point.x + 1) * 1000 + (currentPoint.point.y + 1) * 4 + currentDirection.value)
  }

  def traverseMap(scene: Scene, commands: Seq[Command]) = {
    var currentPoint = scene.arr(0).find(_.isInstanceOf[Walkable]).get.asInstanceOf[Walkable]
    var currentDirection: Direction = Right
    var journeyLog: Map[Point, Direction] = Map(currentPoint.point -> currentDirection)

    for (c <- commands) {
      c match {
        case Move(steps) => {
          (0 until steps).foreach { _ =>
            currentPoint = currentPoint.move(currentDirection)
            journeyLog = journeyLog + (currentPoint.point -> currentDirection)
          }
        }
        case Rotate(rotation) => {
          currentDirection = currentDirection.rotate(rotation)
          journeyLog = journeyLog + (currentPoint.point -> currentDirection)
        }
      }
    }
    println((currentPoint.point.x + 1) * 1000 + (currentPoint.point.y + 1) * 4 + currentDirection.value)
  }

  def createCommands(s: Array[String]): Seq[Command] = {
    s(0).replace("R", " R ")
      .replace("L", " L ")
      .split(" ").map {
      case "R" => Rotate(RightRotation)
      case "L" => Rotate(LeftRotation)
      case steps => Move(steps.toInt)
    }
  }

  private def readMap(): (Scene, Seq[Command]) = {
    val lines = readLinesSplitEmptyLine("day22.txt")
    (createScene(lines(0)), createCommands(lines(1)))
  }

  def getWalkableInDirectionOnFlatMap(arr: ArrayBuffer[ArrayBuffer[Element]], point: Point, vector: Vect): Option[Walkable] = {
    def wrapPoint(p: Point): Point = {
      p.copy(x = (p.x + arr.length) % arr.length, y = (p.y + arr(0).length) % arr(0).length)
    }

    var tmpPoint = wrapPoint(point.move(vector))
    while (!arr.at(tmpPoint).isInstanceOf[Walkable]) {
      if (arr.at(tmpPoint).isInstanceOf[Element.Wall.type]) {
        return None
      }
      tmpPoint = wrapPoint(tmpPoint.move(vector))
    }
    Some(arr.at(tmpPoint).asInstanceOf[Walkable])
  }

  private def createScene(map: Array[String]): Scene = {
    val arr = ArrayBuffer.fill(map.length)(ArrayBuffer.empty[Element])

    fillScene(map, arr)

    for (i <- arr.indices) {
      for (j <- arr(i).indices) {
        val elem = arr(i)(j)
        elem match {
          case w: Walkable => {
            val point = Point(i, j)
            w.up = getWalkableInDirectionOnFlatMap(arr, point, Vect(-1, 0))
            w.down = getWalkableInDirectionOnFlatMap(arr, point, Vect(1, 0))
            w.right = getWalkableInDirectionOnFlatMap(arr, point, Vect(0, 1))
            w.left = getWalkableInDirectionOnFlatMap(arr, point, Vect(0, -1))
            w.point = point
          }
          case _ =>
        }
      }
    }
    Scene(arr)
  }

  private def fillScene(map: Array[String], arr: ArrayBuffer[ArrayBuffer[Element]]): Unit = {
    // initial fill
    for ((line, row) <- map zip arr) {
      row.addAll({
        line.split("").map {
          case " " => Empty
          case "." => Walkable.draft
          case "#" => Wall
        }
      })
    }

    // fill map with empty on right side
    val maxLen = arr.map(_.length).max
    arr.foreach { row => if (row.length < maxLen) row.addAll(Array.fill(maxLen - row.length)(Empty)) }
  }

  case class Scene(arr: ArrayBuffer[ArrayBuffer[Element]]) {
    def show(journeyLog: Map[Point, Direction]): Unit = {
      for (i <- arr.indices) {
        for (j <- arr(i).indices) {
          journeyLog.get(Point(i, j))
            .fold(print(arr(i)(j)))(print(_))
        }
        println()
      }
      println()
    }
  }

  type GetWalkableInDirection = (ArrayBuffer[ArrayBuffer[Element]], Point, Vect) => Option[Walkable]

  trait Direction {
    def rotate(rotation: Rotation): Direction

    def value: Int
  }

  case object Left extends Direction {
    override def toString: String = "<"

    override def rotate(rotation: Rotation): Direction = rotation match {
      case RightRotation => Up
      case LeftRotation => Down
    }

    override def value: Int = 2
  }

  case object Right extends Direction {
    override def toString: String = ">"

    override def rotate(rotation: Rotation): Direction = rotation match {
      case RightRotation => Down
      case LeftRotation => Up
    }

    override def value: Int = 0
  }

  case object Up extends Direction {
    override def toString: String = "^"

    override def rotate(rotation: Rotation): Direction = rotation match {
      case RightRotation => Right
      case LeftRotation => Left
    }

    override def value: Int = 3

  }

  case object Down extends Direction {
    override def toString: String = "v"

    override def rotate(rotation: Rotation): Direction = rotation match {
      case RightRotation => Left
      case LeftRotation => Right
    }

    override def value: Int = 1
  }

  trait Command

  case class Move(steps: Int) extends Command

  case class Rotate(rotation: Rotation) extends Command

  trait Rotation

  object Rotation {
    case object LeftRotation extends Rotation

    case object RightRotation extends Rotation
  }

  trait Element

  object Element {
    case object Empty extends Element {
      override def toString: String = " "
    }

    case class Walkable(var left: Option[Walkable], var right: Option[Walkable], var up: Option[Walkable], var down: Option[Walkable], var point: Point) extends Element {
      def move(direction: Direction): Walkable = {
        val moved = direction match {
          case Right => right
          case Left => left
          case Down => down
          case Up => up
        }
        moved.getOrElse(this)
      }

      override def toString: String = "."
    }

    object Walkable {
      def draft: Walkable = Walkable(null, null, null, null, null)
    }

    case object Wall extends Element {
      override def toString: String = "#"
    }
  }
}
