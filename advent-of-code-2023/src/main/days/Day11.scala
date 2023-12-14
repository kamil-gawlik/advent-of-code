package days

import utils._

object Day11 {

  type Arr = Array[Array[Char]]

  def readInput(): Arr =
    readLines("day11.txt")
      .map(_.toCharArray)

  def expand(arr: Arr): Arr = {
    val emptyXs = arr.zipWithIndex.filterNot(l => l._1.contains('#')).map(_._2)
    val emptyYs = arr.transpose.zipWithIndex.filterNot(l => l._1.contains('#')).map(_._2)

    def grow(arr: Arr, toDuplicate: Seq[Int]): Arr = {
      var res = Array.empty[Array[Char]]
      for (i <- arr.indices) {
        res = res :+ arr(i)
        if (toDuplicate.contains(i)) {
          res = res :+ arr(i)
        }
      }
      res
    }

    val arr2 = grow(arr, emptyXs)
    val arr3 = grow(arr2.transpose, emptyYs)
    arr3.transpose
  }

  def findAllPoints(arr: Arr): Seq[Point] = {
    var res = Seq.empty[Point]
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        if (arr(i)(j) == '#') {
          res = res :+ Point(i, j)
        }
      }
    }
    res
  }

  def part1(): Int = {
    val arr = expand(readInput())
    val allPoints = findAllPoints(arr)
    val paths = allPoints.combinations(2)
      .map { p =>
        val (p1, p2) = (p(0), p(1))
        val path = Math.abs(p1.x - p2.x) + Math.abs((p1.y - p2.y))
        path
      }.toSeq
    println(paths)
    paths.sum
  }

  def part2(): BigInt = {
    val multiplier: BigInt = 1000000 - 1
    val arr = readInput()
    val emptyXs = arr.zipWithIndex.filterNot(l => l._1.contains('#')).map(_._2)
    val emptyYs = arr.transpose.zipWithIndex.filterNot(l => l._1.contains('#')).map(_._2)
    val allPoints = findAllPoints(arr)

    def getRange(i: Int, j: Int): Range = {
      if (i < j) {
        Range(i, j)
      } else Range(j, i)
    }

    val paths = allPoints.combinations(2)
      .map { p =>
        val (p1, p2) = (p(0), p(1))
        val xRange = getRange(p1.x, p2.x)
        val yRange = getRange(p1.y, p2.y)
        val numOfX = emptyXs.count(xRange.contains)
        val numOfY = emptyYs.count(yRange.contains)
        val pathX = Math.abs(p1.x - p2.x) + (numOfX * multiplier)
        val pathY = Math.abs(p1.y - p2.y) + (numOfY * multiplier)
        val path = pathX + pathY
        //println(f"p1 $p1, p2 $p2, xs $numOfX ys: $numOfY, pX: $pathX, pY: $pathY")
        path
      }.toSeq
    paths.sum
  }

  def main(args: Array[String]): Unit = {
    print(part2())
  }
}
