package days

import utils.*

import scala.collection.*
import scala.collection.immutable.Seq

object Day03 {

  def readInput: Array[Array[Char]] = {
    readLines("day03.txt")
      .map(_.toCharArray)
  }

  def scann2(arr: Array[Array[Char]]): (Seq[(Point, String)], Seq[Point]) = {
    var numbers = Seq.empty[(Point, String)]
    var symbols = Seq.empty[Point]
    // read symbols
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        arr(i)(j) match {
          case b if b == '*' => symbols = symbols :+ Point(i, j)
          case _ =>
        }
      }
    }
    // read numbers
    for (i <- arr.indices) {
      val row = arr(i).mkString
      val candidates = row.split(""".!@#$%^&*()-=/+""".toCharArray).filter(_.nonEmpty).map(_.filter(_.isDigit))
      var found = Seq.empty[(Point, String)]
      var offset = 0
      for (n <- candidates) {
        val y = row.indexOf(n, offset)
        found = found :+ (Point(i, y), n)
        offset = y + n.length
      }
      numbers = numbers ++ found
    }

    (numbers, symbols)
  }

  def scann(arr: Array[Array[Char]]): (Seq[(Point, String)], Seq[Point]) = {
    var numbers = Seq.empty[(Point, String)]
    var symbols = Seq.empty[Point]
    // read symbols
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        arr(i)(j) match {
          case b if b != '.' && !b.isDigit => symbols = symbols :+ Point(i, j)
          case _ =>
        }
      }
    }
    // read numbers
    for (i <- arr.indices) {
      val row = arr(i).mkString
      val candidates = row.split(""".!@#$%^&*()-=/+""".toCharArray).filter(_.nonEmpty).map(_.filter(_.isDigit))
      var found = Seq.empty[(Point, String)]
      var offset = 0
      for (n <- candidates) {
        val y = row.indexOf(n, offset)
        found = found :+ (Point(i, y), n)
        offset = y + n.length
      }
      numbers = numbers ++ found
    }

    (numbers, symbols)
  }

  def part1(): Int = {
    val (numbers, symbols) = scann(readInput)
    // points adjacent to symbols
    val symbolAdj: Set[Point] = symbols.flatMap(p => p.neighborsAdjacent).toSet
    val allNumberPoints = numbers.map(_.swap)
      .map {
        p => p._1 -> p._2.allWithin(Vect(0, p._1.length)).toSet
      }
    allNumberPoints
      .filter(p => p._2.intersect(symbolAdj).nonEmpty)
      .flatMap(_._1.toIntOption)
      .sum
  }

  def part2(): Int = {
    val (numbers, symbols) = scann2(readInput)
    // points adjacent to symbols
    val symbolAdj = symbols.map(p => p -> p.neighborsAdjacent).toMap
    val allNumberPoints = numbers.map(_.swap)
      .map {
        p => p._1 -> p._2.allWithin(Vect(0, p._1.length)).toSet
      }

    symbolAdj.values.map { s => {
      val candidates = allNumberPoints.filter(p => p._2.intersect(s.toSet).nonEmpty)
      if (candidates.length == 2) {
        candidates.map(_._1.toInt).product
      } else 0
    }
    }.sum
  }

  def main(args: Array[String]): Unit = {
    check(part1, 525181)
    check(part2, 84289137)
  }
}