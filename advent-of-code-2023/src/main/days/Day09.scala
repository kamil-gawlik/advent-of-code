package days


import utils._

object Day09 {

  def readData(): Array[Seq[Long]] = {
    readLines("day09.txt")
      .map(_.split(" ").map(_.toLong))
  }

  def part1(): Long = {
    val data = readData()

    def findNext(d: Seq[Long]): Long = {
      var row = d
      var pyramid = Seq.empty[Seq[Long]]
      pyramid = pyramid :+ row
      while (row.exists(_ != 0)) {
        val nextRow = row.sliding(2).map(e => e(1) - e.head).toSeq
        pyramid = pyramid :+ nextRow
        row = nextRow
      }
      pyramid.map(_.last).sum
    }

    data.map(findNext).sum
  }

  def findNext(d: Seq[Long]): Long = {
    var row = d
    var pyramid = Seq.empty[Seq[Long]]
    pyramid = pyramid :+ row
    while (row.exists(_ != 0)) {
      val nextRow = row.sliding(2).map(e => e(1) - e.head).toSeq
      pyramid = pyramid :+ nextRow
      row = nextRow
    }
    pyramid.map(_.last).sum
  }

  def part2(): Long = {
    val data = readData()
    data.map(d => findNext(d.reverse)).sum
  }

  def main(args: Array[String]): Unit = {
    check(part1,2105961943)
    check(part2,1019)
  }
}
