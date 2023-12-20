package days

import utils._

object Day06 {

  type I = Long

  case class Race(time: Long, bestDist: Long)

  def readInput1(): Seq[Race] = {
    val lines = readLines("day06.txt")

    def handleLine(l: String): Seq[Int] = {
      val numbers = l.split(":").tail(0)
      numbers.split(" ").map(_.trim).filter(_.nonEmpty).map(_.toInt)
    }

    handleLine(lines(0)).zip(handleLine(lines(1))).map(el => Race(el._1, el._2))
  }

  def readInput2(): Race = {
    val lines = readLines("day06.txt")

    def handleLine(l: String): Long = {
      val numbers = l.split(":").tail(0)
      numbers.replace(" ", "").trim.toLong
    }

    Race(handleLine(lines(0)), handleLine(lines(1)))
  }

  def part1(): Long = {
    val races = readInput1()

    races.map { r =>
      0L.until(r.time).map(e => e * (r.time - e)).count(_ > r.bestDist)
    }.product

  }

  def part2(): Long = {
    val race = readInput2()
    0L.until(race.time).map(e => e * (race.time - e)).count(_ > race.bestDist)
  }

  def main(args: Array[String]): Unit = {
    check(part1, 32076)
    check(part2, 34278221)
  }
}
