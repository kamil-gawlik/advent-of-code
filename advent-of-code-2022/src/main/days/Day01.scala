package days

import utils._

object Day01 {

  def main(args: Array[String]): Unit = {
    // part1
    part2
  }

  private def part2(): Unit = {
    val result = getElfPacks("day01.txt")
      .map(_.sum)
      .sorted
      .reverse
      .take(3)
      .sum
    println(result)
  }

  private def part1(): Unit = {
    val result = getElfPacks("day01.txt")
      .map(_.sum)
      .max
    println(result)
  }

  def getElfPacks(file: String): Array[Array[Int]] = {
    readLinesSplitEmptyLine(file)
      .map(_.map(_.toInt))
  }


}