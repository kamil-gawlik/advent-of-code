package days

import utils._

object Day04 {

  case class Card(id: Int, winning: Seq[Int], yours: Seq[Int])

  object Card {
    val pattern = """Card\s+(\d+): (.+)\|(.+)""".r

    def parseNumbersLine(s: String): Seq[Int] = s.trim.split(" ").filter(_.nonEmpty).map(_.toInt)

    def of(s: String): Card = s match {
      case pattern(cardId, winning, yours) =>
        Card(cardId.toInt, parseNumbersLine(winning), parseNumbersLine(yours))
    }
  }

  def redCards(): Seq[Card] = {
    readLines("day04.txt")
      .map(Card.of)
  }

  def part1(): Int = {
    val cards = redCards()
    cards.map { c =>
        c.winning.foldLeft(0) { (acc, num) =>
          if (c.yours.contains(num))
            if (acc == 0)
              1
            else 2 * acc
          else acc
        }
      }
      //.map { p => println(p); p }
      .sum
  }

  def part2(): Int = {
    val cards = redCards()
    val cardsWithWon = cards.map { c =>
      val won = c.winning.foldLeft(0) { (acc, num) =>
        if (c.yours.contains(num))
          acc + 1
        else acc
      }
      c -> won
    }
    var count: Array[Int] = Array.fill(cards.length)(1)
    for ((c, idx) <- cardsWithWon.zipWithIndex) {
      val (low, high) = count.splitAt(idx+1)
      val (updated, remaining) = high.splitAt(c._2)
      count = low ++ updated.map(_ + count(idx)) ++ remaining
    }
    //println(count.mkString("(", ", ", ")"))
    count.sum
  }

  def main(args: Array[String]): Unit = {
    check(part1,21558)
    check(part2,10425665)
  }
}
