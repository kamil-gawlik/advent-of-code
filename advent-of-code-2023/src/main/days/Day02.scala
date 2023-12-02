package days

import scala.util.matching.Regex
import utils._

object Day02 {
  sealed trait Color

  case object RED extends Color

  case object GREEN extends Color

  case object BLUE extends Color

  object Color {
    def of(s: String): Color = s match {
      case "red" => RED
      case "green" => GREEN
      case "blue" => BLUE
    }
  }

  case class Subset(red: Int, green: Int, blue: Int)

  object Subset {
    def of(s: Seq[(Color, Int)]): Subset = {
      val map = s.toMap
      Subset(map.getOrElse(RED, 0), map.getOrElse(GREEN, 0), map.getOrElse(BLUE, 0))
    }
  }

  case class Game(id: Int, results: Seq[Subset])

  object Game {
    val gamePattern: Regex = "Game (\\d+): (.+)".r
    val resultPattern: Regex = "(\\d+) (.+)".r

    // "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    def of(s: String): Game = {
      s match {
        case gamePattern(gameId, rest) => {
          val results = rest.split(";").map { subset =>
            val sub = subset.split(",").map(_.trim).map {
              case resultPattern(num, color) => (Color.of(color), num.toInt)
            }
            Subset.of(sub)
          }
          Game(gameId.toInt, results)
        }
      }
    }

  }

  def readGames(s: String): Seq[Game] = {
    readLines("day02.txt")
      .map(Game.of)
  }

  def isPossible(game: Game, subset: Subset): Boolean = {
    !game.results.exists { r =>
      r.red > subset.red ||
        r.green > subset.green ||
        r.blue > subset.blue
    }
  }

  // Determine which games would have been possible if the bag had been loaded with only
  // 12 red cubes, 13 green cubes, and 14 blue cube
  def part1(): Int = {
    val games = readGames("day02.txt")
    val refSubset = Subset(12, 13, 14)
    games.filter(isPossible(_, refSubset))
      .map(_.id)
      .sum

  }

  def findMinimalSubset(g: Game): Subset = {
    val acc = Subset(0, 0, 0)
    g.results.foldLeft(acc)((acc, curr) =>
      val red = if (acc.red > curr.red) acc.red else curr.red
      val green = if (acc.green > curr.green) acc.green else curr.green
      val blue = if (acc.blue > curr.blue) acc.blue else curr.blue
      Subset(red, green, blue)
    )
  }

  // 48, 12, 1560, 630, 36
  def part2(): Int = {
    def subsetPower(s: Subset) = s.red * s.green * s.blue

    val games = readGames("day02.txt")
    games.map(findMinimalSubset)
      .map(subsetPower)
      .sum
  }

  def main(args: Array[String]): Unit = {
    println(part2())
  }
}
