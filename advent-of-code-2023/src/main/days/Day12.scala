package days

import utils._

object Day12 {

  case class Row(arr: String, failed: Seq[Int]) {
    def countPossiblePermutations(): Long = {
      countMatches(arr, failed)
    }

    var memo = collection.mutable.Map.empty[(String, Seq[Int]), Long]

    def countMatches(pattern: String, failed: Seq[Int]): Long = {
      val m = memo.get((pattern, failed))
      if (m.isDefined) {
        return m.get
      }
      if (failed.isEmpty) {
        if (!pattern.contains("#")) {
          return 1
        } else return 0
      }

      var count: Long = 0
      val searchedF = failed.head
      val remainingF = failed.tail
      for (i <- Range(0, pattern.length - remainingF.sum - searchedF + 1)) {
        val possible = "." * i + "#" * searchedF + "."
        val matches = pattern.zip(possible).map(e => (e._1 == e._2 || e._1 == '?')).reduce(_ & _)
        if (matches) {
          count = count + countMatches(pattern.drop(possible.length), remainingF)
        }
      }
      memo.put((pattern, failed), count)
      count
    }
  }


  def readInput(): Seq[Row] = {
    readLines("day12.txt")
      .map { l =>
        val ll = l.split(" ")
        val (map, failed) = (ll(0), ll(1))
        Row(map, failed.split(",").map(_.toInt))
      }
  }

  def readInput2(): Seq[Row] = {
    readLines("day12.txt")
      .map { l =>
        val ll = l.split(" ")
        val map = (ll(0) + "?").repeat(4) + ll(0)
        val failed = (ll(1) + ",").repeat(4) + ll(1)
        Row(map, failed.split(",").map(_.toInt))
      }
  }

  def part1(): Long = {
    val rows = readInput()
    rows.map(_.countPossiblePermutations()).sum
  }

  def part2(): Long = {
    val rows = readInput2()
    rows.map(_.countPossiblePermutations()).sum
  }


  def main(args: Array[String]): Unit = {
    check(part1,7633)
    check(part2,23903579139437L)
  }
}
