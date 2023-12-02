package days

import utils._

object Day02 {
  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  def part1(): Unit = {
    val allRounds = readLines("day02.txt")
      .map(_.split(" ").map(Figure.of))
      .map(a => (a(0), a(1))) // change to tuple to make it consistent
    val result = allRounds
      .map(round => (Figure.result(round._1, round._2) + round._2.score))
      .sum
    println(result)
  }

  def part2(): Unit = {
    val allRounds = readLines("day02-2.txt")
      .map(_.split(" ").toList)
      .map(a =>
        (Figure.of(a(0)), Result.of(a(1))) // change to tuple to make it consistent
      )
    val result = allRounds
      .map(round =>
        (round._2.score /*outcome value*/ + Figure.moveToGetResult(round._1, round._2).score /*move value*/)
      ).sum
    println(result)
  }

  trait Figure {
    def score: Int

    def beats: Figure

    def losesTo: Figure
  }

  case object Rock extends Figure {
    override def score: Int = 1

    override def beats: Figure = Scissors

    override def losesTo: Figure = Paper
  }

  case object Paper extends Figure {
    override def score: Int = 2

    override def beats: Figure = Rock

    override def losesTo: Figure = Scissors
  }

  case object Scissors extends Figure {
    override def score: Int = 3

    override def beats: Figure = Paper

    override def losesTo: Figure = Rock
  }

  trait Result {
    def score: Int
  }

  case object Win extends Result {
    override def score: Int = 6
  }

  case object Draw extends Result {
    override def score: Int = 3
  }

  case object Loss extends Result {
    override def score: Int = 0
  }

  object Result {
    def of(s: String): Result = s match {
      case "X" => Loss
      case "Y" => Draw
      case "Z" => Win
    }
  }

  object Figure {
    def of(s: String): Figure = s match {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
    }

    def result(opponent: Figure, you: Figure): Int = (opponent, you) match {
      case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => Win.score
      case (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => Draw.score
      case _ => Loss.score
    }

    def moveToGetResult(opponent: Figure, desiredOutcome: Result): Figure = (opponent, desiredOutcome) match {
      case (f, Win) => f.losesTo
      case (f, Loss) => f.beats
      case (f, Draw) => f
    }
  }
}
