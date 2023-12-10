package days

import utils._

object Day06 {

  type I = BigInt

  case class Race(time: BigInt, bestDist: BigInt)

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

    def handleLine(l: String): BigInt = {
      val numbers = l.split(":").tail(0)
      BigInt(numbers.replace(" ", "").trim)
    }

    Race(handleLine(lines(0)), handleLine(lines(1)))
  }

  def part1(): BigInt = {
    val races = readInput1()

    races.map { r =>
      BigInt(0).until(r.time).map(e => e * (r.time - e)).count(_ > r.bestDist)
    }.product

  }

  def part2(): BigInt = {
    val race = readInput2()
    BigInt(0).until(race.time).map(e => e * (race.time - e)).count(_ > race.bestDist)
  }

  def main(args: Array[String]): Unit = {
    print(part2())
  }
}
