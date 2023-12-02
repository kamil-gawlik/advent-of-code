package days

import utils._

import scala.util.control.Breaks.break

object Day01 {
  def part1(): Int = {
    val lines = readLines("day01.txt")
    lines.map {
        l => l.filter(_.isDigit)
      }.map { s => f"${s.head}${s.last}" }
      .map(_.toInt).sum
  }

  val values = Map(
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

  def part2(): Int = {
    val lines = readLines("day01.txt")
    lines
      .map { l => f"${findFirst(l)}${findLast(l)}" }
      .map { l => l.filter(_.isDigit) }
      .map { l => f"${l.head}${l.last}" }
      .map { l => println(l); l }
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
    return s
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
        //println(f"$cc, $c")
        return c.get.reverse
      }
    }
    return s
  }

  // 29, 83, 13, 24, 42, 14, 76
  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }
}