package days

import utils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day09 {

  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  private def part1(): Unit = {
    val commands: Array[Command] = readLines("day09.txt")
      .map(_.split(" ")).map(arr => Command(Direction.of(arr(0)), arr(1).toInt))

    val tailPositionsBuffer = mutable.Set[Position]()
    var ropePosition = ShortRopePosition(Position.starting, Position.starting)
    tailPositionsBuffer.addOne(Position(0, 0))
    commands.foreach(c => {
      (0 until c.steps).foreach(_ => {
        ropePosition = ropePosition.moveOneStep(c.direction)
        println(ropePosition.tailPosition)
        tailPositionsBuffer.addOne(ropePosition.tailPosition)
      }
      )
    })
    println(tailPositionsBuffer.size)
  }

  private def part2(): Unit = {
    val commands: Array[Command] = readInput

    val tailPositionsBuffer = mutable.Set[Position]()
    var ropePosition = LongRopePosition(Position.starting, Array.fill(9)(Position.starting))
    tailPositionsBuffer.addOne(Position(0, 0))
    commands.foreach(c => {
      (0 until c.steps).foreach(_ => {
        ropePosition = ropePosition.moveOneStep(c.direction)
        println(ropePosition.tailPosition)
        tailPositionsBuffer.addOne(ropePosition.tailPosition)
      }
      )
    })
    println(tailPositionsBuffer.size)
  }

  private def readInput = {
    val commands = readLines("day09.txt")
      .map(_.split(" ")).map(arr => Command(Direction.of(arr(0)), arr(1).toInt))
    commands
  }

  def moveAdjacentKnots(tailPosition: Position, headPosition: Position): Position = {
    def makeOneWIthSign(dx: Int): Int = if (dx == 0) 0 else (dx / Math.abs(dx))

    (headPosition.x - tailPosition.x, headPosition.y - tailPosition.y) match {
      case (0, 0) => tailPosition // same position
      case (0, dy) if Math.abs(dy) == 1 => tailPosition
      case (dx, 0) if Math.abs(dx) == 1 => tailPosition
      case (dx, dy) if Math.abs(dx) == 1 && Math.abs(dy) == 1 => tailPosition // simply adjacent
      case (dx, dy) => Position(tailPosition.x + makeOneWIthSign(dx), tailPosition.y + makeOneWIthSign(dy))
    }
  }

  case class ShortRopePosition(headPosition: Position, tailPosition: Position) {
    def moveOneStep(direction: Direction): ShortRopePosition = {
      val newHeadPosition = headPosition.changeWithDirectionMove(direction)
      val newTailPosition = moveAdjacentKnots(tailPosition, newHeadPosition)
      ShortRopePosition(newHeadPosition, newTailPosition)
    }
  }

  case class LongRopePosition(headPosition: Position, knotsPositions: Array[Position]) {
    private val length: Int = 9

    def tailPosition: Position = knotsPositions(length-1)

    def moveOneStep(direction: Direction): LongRopePosition = {
      val newHeadPosition = headPosition.changeWithDirectionMove(direction)
      val newKnotsPositions = ArrayBuffer[Position]()
      knotsPositions
        .foldLeft(newHeadPosition)((prevHead, currTail) => {
          val newKnotPosition = moveAdjacentKnots(currTail, prevHead)
          newKnotsPositions.addOne(newKnotPosition)
          newKnotPosition
        })
      LongRopePosition(newHeadPosition, newKnotsPositions.toArray)
    }
  }

  case class Position(x: Int, y: Int) {
    def changeWithDirectionMove(direction: Direction): Position = direction match {
      case L => Position(x - 1, y)
      case R => Position(x + 1, y)
      case U => Position(x, y + 1)
      case D => Position(x, y - 1)
    }
  }

  object Position {
    val starting = Position(0, 0)
  }

  case class Command(direction: Direction, steps: Int)

  trait Direction

  case object L extends Direction

  case object R extends Direction

  case object U extends Direction

  case object D extends Direction

  object Direction {
    def of(s: String): Direction = s match {
      case "L" => L
      case "R" => R
      case "U" => U
      case "D" => D
      case _ => throw new UnsupportedOperationException()
    }
  }
}
