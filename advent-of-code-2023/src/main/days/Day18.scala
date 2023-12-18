package days

import utils.{Point, *}
import utils.Vect.*

object Day18 {

  val commands = readLines("day18.txt").map(Command.of)

  case class Point(x: Long, y: Long) {
    def move(v: Vect): Point = Point(x + v.x, y + v.y)
  }

  case class Command(dir: Vect, steps: Int, rgb: String) {
    def rgbed(): Command = {
      val (steps, dir2) = rgb.splitAt(5)

      Command(Command.dirFromS(dir2), BigInt(steps, 16).toInt, rgb)
    }
  }

  object Command {
    val pattern = """(.+) (.+) \(#(.+)\)""".r

    private def dirFromS(s: String): Vect = s match {
      case "R" | "0" => RIGHT
      case "D" | "1" => DOWN
      case "L" | "2" => LEFT
      case "U" | "3" => UP
    }

    def of(s: String): Command = s match {
      case pattern(dir, steps, rgb) =>
        Command(dirFromS(dir), steps.toInt, rgb)
    }

  }

  def shoeLaceArea(vertices: Seq[Point]): BigInt = {
    val res = vertices.sliding(2)
      .map { el =>
        val (u, v) = (el(0), el(1))
        u.x * v.y - u.y * v.x
      }.sum / 2
    Math.abs(res)
  }

  def integerPointsFromPickTheorem(area: BigInt, perimeter: BigInt): BigInt =
    area + (perimeter / 2) + 1

  private def solve(commands: Seq[Command]) = {
    val startingPoint = Point(0L, 0L)
    var vertices = Seq.empty[Point]
    var perimeter = 0L
    var point = startingPoint
    for (c <- commands) {
      point = point.move(c.dir.scale(c.steps))
      perimeter = perimeter + c.steps
      vertices = vertices :+ point
    }
    val area = shoeLaceArea(vertices)
    integerPointsFromPickTheorem(area, perimeter)
  }

  def part1(): BigInt = {
    solve(commands)
  }

  def part2(): BigInt = {
    solve(commands.map(_.rgbed()))
  }

  def main(args: Array[String]): Unit = timeMs {
    println(part1()) //48795
    println(part2()) // 40654918441248
  }
}
