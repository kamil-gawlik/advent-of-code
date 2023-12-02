package days

import utils._

object Day06 {

  def main(args: Array[String]): Unit = {
    part1()
  }

  def part1(): Unit = {
    val input = readLines("day06.txt")(0)
    var counter = 0
    input.sliding(14)
      .foreach(s => {
        counter = counter + 1
        if(s.toArray.toSet.size == 14){println(counter+13)}
      }
      )
  }

  def part2(): Unit = ???

}
