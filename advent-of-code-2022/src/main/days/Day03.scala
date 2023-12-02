package days

import utils._

object Day03 {

  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

  private def part1(): Unit = {
    def findCommonInLine(l: String): Char = {
      val (x1, x2) = l.splitAt(l.length / 2)
      (x1.toCharArray.toSet).intersect(x2.toCharArray.toSet).head
    }

    val lines = readLines("day03.txt")
    val result = lines.map(findCommonInLine)
      .map(charValue)
      .sum
    println(result)
  }

  def part2(): Unit = {
    def findCommonInLines(a: Array[String]): Char = {
      val sets = a.map(_.toCharArray.toSet)
      sets(0).intersect(sets(1)).intersect(sets(2)).head
    }

    val result = readLines("day03-2.txt")
      .grouped(3)
      .map(findCommonInLines)
      .map(charValue)
      .sum
    println(result)
  }


  private def charValue(c: Char): Int =
    if (c.isLower) {
      c - 'a' + 1
    } else {
      c - 'A' + 27
    }
}

