package days

import utils._

object Day25 {

  def main(args: Array[String]): Unit = {
    part1()
  }

  private def part1(): Unit = {
    val lines = readLines("day25.txt")
    println(encode(lines.map(decode).sum))
  }

  private def decode(s: String): Long = {
    s.reverse.foldLeft((0L, 0L)) { case ((sum, position), c) =>
      (sum + Math.pow(5, position).toLong * charToLong(c), position + 1)
    }._1
  }

  private def encode(l: Long): String = {
    def rec2(acc: String, num: Long): String = {
      (num / 5, num % 5) match {
        case (l, r) if l == 0 && r == 0 => acc
        case (l, r) if r > 0 && r < 3 =>
          rec2(acc + r, l)
        case (l, r) if r == 3 =>
          rec2(acc + "=",
            ((num + 2) / 5)
          )
        case (l, r) if r == 4 =>
          rec2(acc + "-",
            ((num + 1) / 5)
          )
        case (l, r) => rec2(acc + r, l)
      }
    }

    rec2("", l).reverse
  }

  private def charToLong(c: Char): Long = c match {
    case '-' => -1L
    case '=' => -2L
    case _ => c.toString.toLong
  }


}
