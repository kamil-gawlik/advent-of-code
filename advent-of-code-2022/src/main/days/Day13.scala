package days

import utils._

import scala.annotation.tailrec

object Day13 {
  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  private def part1(): Unit = {
    val lines = readLines("day13.txt").filter(_.nonEmpty).map(_.replace("10", "A"))
    val res = lines.grouped(2).zipWithIndex
      .collect {
        case (pair, idx) if (compare(pair(0), pair(1))) => idx + 1
      }.sum
    println(res)
  }

  private def part2(): Unit = {
    val d1 = "[[2]]"
    val d2 = "[[6]]"
    val lines = readLines("day13.txt").filter(_.nonEmpty).map(_.replace("10", "A"))
    val sorted = (lines :+ d1 :+ d2).sortWith(compare)
    val res = sorted.zipWithIndex collect { case (s, idx) if (s == d1 || s == d2) => idx + 1 }
    println(res.product)
  }

  @tailrec
  private def compare(s1: String, s2: String): Boolean = {
    //println(s"$s1\n$s2\n\n")
    (s1.head, s2.head) match {
      case (a, b) if a == b => compare(s1.tail, s2.tail)
      case (']', _) => true
      case (_, ']') => false
      case ('[', b) => compare(s1.tail, b + "]" + s2.tail)
      case (a, '[') => compare(a + "]" + s1.tail, s2.tail)
      case (a, b) => a < b

    }
  }


}
