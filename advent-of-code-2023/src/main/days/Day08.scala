package days

import utils._

object Day08 {

  case class Next(l: String, r: String) {
    def get(s: String): String =
      if (s == "L") {
        l
      } else r
  }

  def readData(): (Seq[String], Map[String, Next]) = {
    val data = readLinesSplitEmptyLine("day08.txt")
    val directions = data(0)(0).split("")
    var map = Map.empty[String, Next]
    val pattern = """(.+) = \((.+), (.+)\)""".r
    for (l <- data(1)) {
      l match
        case pattern(k, l, r) =>
          map = map + (k -> Next(l, r))
    }
    (directions, map)
  }

  def part1(): Int = {
    val (directions, map) = readData()
    var c = 0
    var curr = "AAA"
    for (d <- Array.fill(100)(directions).flatten) {
      c = c + 1
      curr = map(curr).get(d)
      if (curr == "ZZZ") {
        return c
      }
    }
    return -1
  }

  def gcd(x: BigInt, y: BigInt): BigInt =
    if (y == 0)
      x
    else
      gcd(y, x % y)

  def lcm(numbers: Seq[BigInt], index: Int): BigInt = {
    if (index == numbers.length - 1) {
      return numbers(index)
    }
    val a = numbers(index)
    val b = lcm(numbers, index + 1)
    (a * b) / gcd(a, b)
  }

  def part2(): BigInt = {
    val (directions, map) = readData()

    def getNumberOfStepsToZ(startPoint: String, map: Map[String, Next]): BigInt = {
      var c = BigInt(0)
      var curr = startPoint
      for (d <- Array.fill(10000)(directions).flatten) {
        c = c + 1
        curr = map(curr).get(d)
        if (curr.endsWith("Z")) {
          return c
        }
      }
      -1
    }

    val startingPoints = map.keys.filter(_.endsWith("A"))
    // works on assumption, there is only one end for each starting node
    val stepsToZ = startingPoints.map(c => getNumberOfStepsToZ(c, map))
    lcm(stepsToZ.toSeq, 0)
  }

  def main(args: Array[String]): Unit = {
    println(part2())
  }
}
