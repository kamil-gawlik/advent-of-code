package days

import days.Day23.Directions._
import utils._

object Day23 {

  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  private def part2(): Unit = {
    var prev = Set.empty[Point]
    var elfs = readElfs()
    var moveCounter = 0
    //show(elfs)
    while (elfs != prev) {
      timeMs {
        val newPositions = elfs.map(elfPosition => elfPosition -> proposeNewPosition(moveCounter, elfPosition, elfs)).toMap
        val (noClashes, clash) = newPositions.groupBy(_._2).partition(_._2.size <= 1) // grouped by goal position
        val originalPositionForClashed = clash.flatMap(_._2.keySet)
        val result = (noClashes.keySet ++ originalPositionForClashed)
        println(moveCounter + 1, elfs.size)
        //show(result)
        prev = elfs
        elfs = result
        moveCounter = moveCounter + 1
      }
    }
    println()
    println(moveCounter)
  }

  private def part1(): Unit = {
    val elfs = readElfs()
    //show(elfs)
    val result = (0 until 10).foldLeft(elfs) { (elfs, moveCounter) =>
      timeMs {
        val newPositions = elfs.map(elfPosition => elfPosition -> proposeNewPosition(moveCounter, elfPosition, elfs)).toMap
        val (noClashes, clash) = newPositions.groupBy(_._2).partition(_._2.size <= 1) // grouped by goal position
        val originalPositionForClashed = clash.flatMap(_._2.keySet)
        val result = (noClashes.keySet ++ originalPositionForClashed)
        println(moveCounter + 1)
        //show(result)
        result
      }
    }
    val xDim = Math.abs(result.map(_.x).max - result.map(_.x).min) + 1
    val yDim = Math.abs(result.map(_.y).max - result.map(_.y).min) + 1
    println(xDim * yDim - result.size)
  }

  private def show(value: Set[Point]): Unit = {
    val xs = value.map(_.x)
    val ys = value.map(_.y)
    for (x <- xs.min to xs.max) {
      for (y <- ys.min to ys.max) {
        if (value.contains(Point(x, y))) {
          print("#")
        } else {
          print(".")
        }
      }
      println()
    }
    println()
  }

  /*
    If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
    If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
    If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
    If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.
   */
  private def proposeNewPosition(moveCounter: Int, point: Point, elfs: Set[Point]): Point = {
    val zeroRule = (point: Point) => Set(
      point.move(N), point.move(NE), point.move(NW),
      point.move(S), point.move(SE), point.move(SW),
      point.move(W), point.move(E)
    )
    val rules: Seq[(Point => Set[Point], Vect)] = Seq(
      ((point: Point) => Set(point.move(N), point.move(NE), point.move(NW))) -> N,
      ((point: Point) => Set(point.move(S), point.move(SE), point.move(SW))) -> S,
      ((point: Point) => Set(point.move(W), point.move(NW), point.move(SW))) -> W,
      ((point: Point) => Set(point.move(E), point.move(NE), point.move(SE))) -> E
    )

    val elfsSet = elfs.toSet
    if (zeroRule(point).intersect(elfsSet).isEmpty) {
      point
    } else {

      val possibleMoves = (rules.indices.to(LazyList).map { idx =>
        val i = (moveCounter + idx) % rules.size
        val (checkPoints, move) = rules(i)
        val checkedPositions = checkPoints(point)
        if (elfsSet.intersect(checkedPositions).isEmpty) {
          Some(point.move(move))
        } else {
          None
        }
      } find (_.isDefined)
        ).flatten
      possibleMoves.getOrElse(point)
    }
  }

  private def readElfs(): Set[Point] = {
    val arr = readLines("day23.txt")
    arr.zipWithIndex.flatMap {
      case (coll, rowIdx) =>
        coll.zipWithIndex.map {
          case (filed, collIdx) => if (filed == '#') Some(Point(rowIdx, collIdx)) else None
        }
    }.flatten.toSet
  }


  object Directions {
    val N: Vect = Vect.UP
    val S: Vect = Vect.DOWN
    val E: Vect = Vect.RIGHT
    val W: Vect = Vect.LEFT

    val NE: Vect = Vect(-1, 1)
    val NW: Vect = Vect(-1, -1)
    val SE: Vect = Vect(1, 1)
    val SW: Vect = Vect(1, -1)

    val ZERO: Vect = Vect(0, 0)
  }

  case class Decision(vect: Vect, check: Point => Boolean)
}
