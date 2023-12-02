package days

import utils._

object Day04 {
  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

  private def part1(): Unit = {
    val ranges: Array[Array[Array[Int]]] = readInput("day04.txt")
    val res = ranges.count(isOneRangeInclusive)
    println(res)
  }

  private def part2(): Unit = {
    val ranges: Array[Array[Array[Int]]] = readInput("day04-2.txt")
    val res = ranges.count(rangeOverlap)
    println(res)
  }

  def isOneRangeInclusive(a: Array[Array[Int]]): Boolean = {
    def isPartOf(r1: Range, r2: Range) = (r1.start <= r2.start && r1.end >= r2.end)

    val r1 = Range.inclusive(a(0)(0), a(0)(1))
    val r2 = Range.inclusive(a(1)(0), a(1)(1))
    isPartOf(r1, r2) || isPartOf(r2, r1)
  }

  def rangeOverlap(a: Array[Array[Int]]): Boolean = {
    def overlaps(r1: Range, r2: Range) = (r1.end >= r2.start && r1.end <= r2.end)

    val r1 = Range.inclusive(a(0)(0), a(0)(1))
    val r2 = Range.inclusive(a(1)(0), a(1)(1))
    val res = overlaps(r1, r2) || overlaps(r2, r1)
    // if (res) println(s"$r1 $r2")
    res
  }

  private def readInput(f: String): Array[Array[Array[Int]]] = {
    val ranges = readLines(f)
      .map(
        _.split(",") //1-2,3-4 -> (1-2)(3-4)
          .map(_.split("-").map(_.toInt)) // ([1][2])([3][]4)
      )
    ranges
  }

}
