package days

import utils._

import scala.collection.mutable

object Day05 {
  def main(args: Array[String]): Unit = {
    part1()
    part2()
  }

  private def part1(): Unit = {
    val (stacks, commands) = parseData("day05.txt")

    def moveStacks(command: Command): Unit = {
      (0 until command.count).foreach(_ => {
        val moved = stacks(command.from - 1).pop()
        stacks(command.to - 1).push(moved)
      }
      )
    }

    commands.foreach(c =>
      moveStacks(c)
    )
    println(stacks.map(s => s.pop()).mkString)
  }

  private def part2(): Unit = {
    val (stacks, commands) = parseData("day05-2.txt")

    def moveStacks(command: Command): Unit = {
      var tmp: Array[Char] = Array.empty[Char]
      (0 until command.count).foreach(_ => {
        val moved = stacks(command.from - 1).pop()
        tmp = tmp.appended(moved)
      }
      )
      tmp.reverse.map(t => stacks(command.to - 1).push(t))
    }

    commands.foreach(c =>
      moveStacks(c)
    )
    println(stacks.map(s => s.pop()).mkString)
  }


  private def parseData(fileName: String): (Array[mutable.Stack[Char]], Array[Command]) = {
    val lines = readLinesSplitEmptyLine(fileName)

    val stacks = { // stacks setup
      val rows = lines(0).map(ss => ss.grouped(4).toArray) // [Z] [M] [P] => ("[Z] ", "[M] ", "[P])
      val columns = rows.transpose.map(_.reverse) // (...) => ("1", "[Z]", "[N]", " ")
      val stacks: Array[mutable.Stack[Char]] = columns.map { c => {
        val (_, cc) = c.splitAt(1) // ignore stack index
        val s = mutable.Stack.empty[Char]
        cc.foreach(ccc =>
          if (ccc.replace(" ", "").nonEmpty) {
            s.push(ccc.charAt(1))
          }
        )
        s
      }
      }
      stacks
    }

    val commands = lines(1).map(l => {
      val values = l
        .replace("move ", "")
        .replace("from ", "")
        .replace("to ", "")
        .split(" ")
        .map(_.toInt)
      Command(values(0), values(1), values(2))
    })
    (stacks, commands)
  }

  case class Command(count: Int, from: Int, to: Int)
}
