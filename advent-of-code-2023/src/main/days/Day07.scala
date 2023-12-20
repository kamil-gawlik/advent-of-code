package days

import days.Day07.HandValue.{FiveOfAKind, FourOfAKind, FullHouse, HighCard, OnePair, ThreeOfAKind, TwoPair}
import utils.*

import scala.util.Sorting

object Day07 {
  case class Card(symbol: Char, strength: Int)

  object Card {
    def of(s: Char): Card = s match {
      case 'A' => Card(s, 14)
      case 'K' => Card(s, 13)
      case 'Q' => Card(s, 12)
      case 'J' => Card(s, 11)
      case 'T' => Card(s, 10)
      case '9' => Card(s, 9)
      case '8' => Card(s, 8)
      case '7' => Card(s, 7)
      case '6' => Card(s, 6)
      case '5' => Card(s, 5)
      case '4' => Card(s, 4)
      case '3' => Card(s, 3)
      case '2' => Card(s, 2)
    }
  }

  enum HandValue {
    case HighCard extends HandValue
    case OnePair extends HandValue
    case TwoPair extends HandValue
    case ThreeOfAKind extends HandValue
    case FullHouse extends HandValue
    case FourOfAKind extends HandValue
    case FiveOfAKind extends HandValue
  }

  object HandValue {
    def of(h: Hand): HandValue = h.cards match {
      case c if c.hasFive => FiveOfAKind
      case c if c.hasFour => FourOfAKind
      case c if c.hasPair && c.hasTriple => FullHouse
      case c if c.hasTriple => ThreeOfAKind
      case c if c.hasTwoPairs => TwoPair
      case c if c.hasPair => OnePair
      case _ => HighCard
    }
  }

  object HandValueWithJoker {
    def of(h: Hand): HandValue = {
      val jCount = h.cards.count(_.symbol == 'J')
      if (jCount == 5) {
        return FiveOfAKind
      }
      val cardsWithoutJoker = h.cards.filter(_.symbol != 'J')
      val topCard = cardsWithoutJoker.toCount.toSeq.maxBy(_._2)._1
      val cardsSubstituted = cardsWithoutJoker ++ Array.fill(jCount)(topCard)
      cardsSubstituted match {
        case c if c.hasFive => FiveOfAKind
        case c if c.hasFour => FourOfAKind
        case c if c.hasPair && c.hasTriple => FullHouse
        case c if c.hasTriple => ThreeOfAKind
        case c if c.hasTwoPairs => TwoPair
        case c if c.hasPair => OnePair
        case _ => HighCard
      }
    }
  }

  extension (cards: Seq[Card])
    def toCount: Map[Card, Int] = cards.groupBy(c => c).map(l => l._1 -> l._2.length)

    def hasTwoPairs: Boolean = cards.toCount.values.count(_ == 2) == 2
    def hasPair: Boolean = cards.toCount.values.exists(_ == 2)
    def hasTriple: Boolean = cards.toCount.values.exists(_ == 3)
    def hasFour: Boolean = cards.toCount.values.exists(_ == 4)

    def hasFive: Boolean = cards.toCount.values.exists(_ == 5)


  case class Hand(cards: Seq[Card], bid: Int) {
    override def toString: String = cards.map(_.symbol).mkString
  }

  object HandOrdering extends Ordering[Hand] {
    override def compare(x: Hand, y: Hand): Int =
      val ord = HandValue.of(x).ordinal.compare(HandValue.of(y).ordinal)
      if (ord == 0) {
        val comparedCards = x.cards.zip(y.cards).map(el => el._1.strength.compare(el._2.strength))
        comparedCards.find(e => e != 0).getOrElse(0)
      } else {
        ord
      }
  }

  object HandOrderingWithJoker extends Ordering[Hand] {
    def overrideJokersStrength(c: Card): Card =
      if (c.symbol == 'J')
        Card('J', 1)
      else Card(c.symbol, c.strength + 1)

    override def compare(x: Hand, y: Hand): Int =
      val ord = HandValueWithJoker.of(x).ordinal.compare(HandValueWithJoker.of(y).ordinal)
      if (ord == 0) {
        val comparedCards = x.cards.zip(y.cards).map(el => overrideJokersStrength(el._1).strength.compare(overrideJokersStrength(el._2).strength))
        comparedCards.find(e => e != 0).getOrElse(0)
      } else {
        ord
      }
  }

  private def readHands(): Seq[Hand] = {
    val lines = readLines("day07.txt")
    val hands = lines.map { l =>
      val cards = l.split(" ")(0).map(Card.of)
      val bid = l.split(" ")(1).toInt
      Hand(cards, bid)
    }
    hands
  }

  def part1(): Int = {
    val hands = readHands()
    val sorted = hands.sorted(HandOrdering)
    sorted.map(_.bid).zipWithIndex.map(el => el._1 * (el._2 + 1)).sum
  }

  /*
  32T3K one pair
  KK677 two pair
  T55J5 four of a kind
  QQQJA four of a kind
  KTJJT four of a kind
   */
  def part2(): BigInt = {
    val hands = readHands()
    //hands.foreach(h => println(f"$h ${HandValueWithJoker.of(h)}"))
    val sorted = hands.sorted(HandOrderingWithJoker)
    //sorted.foreach(h => println(f"$h ${HandValueWithJoker.of(h)} ${h.bid}"))
    sorted.map(s => BigInt.int2bigInt(s.bid)).zipWithIndex.map(el => el._1 * (el._2 + 1)).sum
  }

  def main(args: Array[String]): Unit = {
    check(part1,248105065)
    check(part2,249515436)
  }
}
