package days


import utils._

object Day09 {

  def readData(): Array[Seq[BigInt]] = {
    readLines("day09.txt")
      .map(_.split(" ").map(BigInt(_)))
  }

  def part1(): BigInt = {
    val data = readData()

    def findNext(d: Seq[BigInt]): BigInt = {
      var row = d
      var pyramid = Seq.empty[Seq[BigInt]]
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

  def findNext(d: Seq[BigInt]): BigInt = {
    var row = d
    var pyramid = Seq.empty[Seq[BigInt]]
    pyramid = pyramid :+ row
    while (row.exists(_ != 0)) {
      val nextRow = row.sliding(2).map(e => e(1) - e.head).toSeq
      pyramid = pyramid :+ nextRow
      row = nextRow
    }
    pyramid.map(_.last).sum
  }

  def part2(): BigInt = {
    val data = readData()
    data.map(d => findNext(d.reverse)).sum
  }

  def main(args: Array[String]): Unit = {
    print(part2())
  }
}
