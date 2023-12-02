package days

import utils._

import scala.collection.mutable

object Day24 {
  def main(args: Array[String]): Unit = {
    part2()
  }

  private def part2(): Unit = {
    var stage = readStage()
    var counter = 1
    val q = mutable.Queue(Point(0, 0))
    stage = stage.tick()
    while (!q.contains(Point(stage.x - 1, stage.y - 1))) {
      stage = stage.tick()
      val nextRound = (q.dequeueAll(_ => true).flatMap(stage.findPossiblePositions(_))).toSet
      q.enqueueAll(nextRound)
      counter = counter + 1
    }
    stage = stage.tick()
    stage.show()
    counter = counter +1
    println(counter + 1)
    q.dequeueAll(_ => true)
    q.enqueue(Point(stage.x - 1, stage.y - 1))
    while (q.nonEmpty && !q.contains(Point(0, 0))) {
      //println(counter)
      stage = stage.tick()
      //stage.show()
      val nextRound = (q.dequeueAll(_ => true).flatMap(stage.findPossiblePositions(_))).toSet
      //println(s"nextPoints: ${nextRound.mkString(",")}")
      q.enqueueAll(nextRound)
      counter = counter + 1
    }
    println(counter + 1)
    q.dequeueAll(_ => true)
    q.enqueue(Point(0, 0))
    while (q.nonEmpty && !q.contains(Point(stage.x - 1, stage.y - 1))) {
      //println(counter)
      stage = stage.tick()
      //stage.show()
      val nextRound = (q.dequeueAll(_ => true).flatMap(stage.findPossiblePositions(_))).toSet
      //println(s"nextPoints: ${nextRound.mkString(",")}")
      q.enqueueAll(nextRound)
      counter = counter + 1
    }

    println(counter + 1)
  }

  private def part1(): Unit = {
    var stage = readStage()
    var counter = 1
    val q = mutable.Queue(Point(0, 0))
    stage = stage.tick()
    while (!q.contains(Point(stage.x - 1, stage.y - 1))) {
      println(counter)
      stage = stage.tick()
      stage.show()
      val nextRound = (q.dequeueAll(_ => true).flatMap(stage.findPossiblePositions(_))).toSet
      //println(s"nextPoints: ${nextRound.mkString(",")}")
      q.enqueueAll(nextRound)
      counter = counter + 1
    }
    println(counter + 1)
  }

  private def readStage(): Stage = {
    val lines = readLines("day24.txt")
      .drop(1).dropRight(1)
      .map(l => l.replace("#", ""))

    val blizzards = lines.zipWithIndex.flatMap {
      case (row, x) => row.split("").zipWithIndex.map {
        case (col, y) => {
          val direction: Option[Direction] = col match {
            case ">" => Some(Right)
            case "<" => Some(Left)
            case "^" => Some(Up)
            case "v" => Some(Down)
            case _ => None
          }
          direction.map(Blizzard(Point(x, y), _))
        }
      }
    }
    Stage(blizzards.flatten.toSeq, lines.length, lines(0).length)
  }


  case class Stage(blizzards: Seq[Blizzard], x: Int, y: Int) {

    def isCorrectSpecial(point: Point, x: Int, y: Int): Boolean =
      (point == Point(-1, 0) || point == Point(x, y - 1)) || point.isCorrect(x, y)

    def findPossiblePositions(point: Point): Seq[Point] = {
      val possiblePoijnts = Seq(Up.vect, Down.vect, Left.vect, Right.vect, Vect(0, 0))
        .map(point.move)
      val filteredCorrect = possiblePoijnts
        .filter(isCorrectSpecial(_, x, y))
      val filteredByBlizard = filteredCorrect
        .filterNot(blizzardsMap.keySet.contains)
      /*println(s"from: $point")
      println(s"possible [${possiblePoijnts.mkString(",")}]")
      println(s"after correct [${filteredCorrect.mkString(",")}]")
      println(s"after blizard [${filteredByBlizard.mkString(",")}]")
      println()*/
      filteredByBlizard
    }

    def tick(): Stage = {
      Stage(
        blizzards.map(b => Blizzard(b.point.move(b.direction.vect).align(x, y), b.direction)),
        x, y
      )
    }

    val blizzardsMap: Map[Point, Direction] = blizzards.map(b => b.point -> b.direction).toMap

    def show(): Unit = {
      println("#" * (y + 2))
      for (i <- 0 until x) {
        print("#")
        for (j <- 0 until y) {
          if (blizzardsMap.contains(Point(i, j))) {
            print(blizzardsMap(Point(i, j)))
          } else {
            print(".")
          }
        }
        println("#")
      }
      println("#" * (y + 2))
      println()
    }
  }


  case class Blizzard(point: Point, direction: Direction)

  trait Direction {
    def vect: Vect

    def toString: String
  }

  case object Left extends Direction {
    override def vect: Vect = Vect(0, -1)

    override def toString: String = "<"
  }

  case object Right extends Direction {
    override def vect: Vect = Vect(0, 1)

    override def toString: String = ">"
  }

  case object Up extends Direction {
    override def vect: Vect = Vect(-1, 0)

    override def toString: String = "^"
  }

  case object Down extends Direction {
    override def vect: Vect = Vect(1, 0)

    override def toString: String = "v"
  }

}
