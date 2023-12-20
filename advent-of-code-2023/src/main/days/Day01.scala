package days

import utils._

import scala.util.control.Breaks.break

object Day01 {
  val lines = readLines("day01.txt")

  private val values: Map[String, Int] = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9
  )

  def part1(): Int = {
    lines.map {
        l => l.filter(_.isDigit)
      }.map { s => f"${s.head}${s.last}" }
      .map(_.toInt).sum
  }

  def part2(): Int = {
    lines
      .map { l => f"${findFirst(l)}${findLast(l)}" }
      .map { l => l.filter(_.isDigit) }
      .map { l => f"${l.head}${l.last}" }
      //.map { l => println(l); l }
      .map(_.toInt).sum
  }

  def findFirst(s: String): String = {
    var c: Option[String] = None
    for (cc <- s.sliding(5)) {
      if (cc.head.isDigit) {
        return cc.head.toString
      }
      c = values.keySet
        .find { vv => cc.contains(vv) }
        .map { found => cc.replace(found, values(found).toString) }
      if (c.nonEmpty) {
        return c.get
      }
    }
    s
  }

  def findLast(s: String): String = {
    var c: Option[String] = None
    for (cc <- s.reverse.sliding(5)) {
      if (cc.head.isDigit) {
        return cc.head.toString
      }
      c = values.keySet.map(_.reverse)
        .find { vv => cc.contains(vv) }
        .map { found => cc.replace(found, values(found.reverse).toString) }
      if (c.nonEmpty) {
        return c.get.reverse
      }
    }
    s
  }

  def main(args: Array[String]): Unit = {
    check(part1, 55108)
    check(part2, 56324)
  }
}