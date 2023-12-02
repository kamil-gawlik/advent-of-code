package days

import utils._

object Day10 {

  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  def part1(): Unit = {
    val commands = readLines("day10.txt").flatMap(Command.of)
    var register_X = 1
    var accumulator = 0
    for (i <- commands.indices) {
      val cycle_number = i + 1
      if (cycle_number == 20 || ((cycle_number - 20) % 40 == 0)) {
        println(s"$cycle_number ${register_X} ${cycle_number * register_X}")
        accumulator = accumulator + (cycle_number * register_X)
      }
      register_X = register_X + commands(i).value
    }
    println(accumulator)
  }

  def part2(): Unit = {
    val commands = readLines("day10.txt").flatMap(Command.of)
    var register_X = 1
    for (i <- commands.indices) {
      val cycle_number = i + 1
      val sprite_position = register_X
      val sprite_range = List(sprite_position - 1, sprite_position, sprite_position + 1)
      if (sprite_range.contains(i % 40)) {
        print("#")
      } else {
        print(".")
      }
      if (cycle_number % 40 == 0) {
        println()
      }
      register_X = register_X + commands(i).value
    }
  }


  trait Command {
    def value: Int
  }

  case class Addx(value: Int) extends Command

  object Noop extends Addx(0)

  object Command {
    private val addx_r = "addx (.+)".r

    def of(s: String): List[Command] = s match {
      case addx_r(v) => List(Noop, Addx(v.toInt)) // symulate two cycles to make it easier
      case "noop" => List(Noop)
    }
  }


}
