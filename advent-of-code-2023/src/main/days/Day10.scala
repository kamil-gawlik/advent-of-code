package days

import utils.*

import scala.collection.immutable.Seq

object Day10 {

  val connections = Map(
    "S" -> Seq(Vect.UP, Vect.DOWN, Vect.RIGHT, Vect.LEFT),
    "|" -> Seq(Vect.UP, Vect.DOWN),
    "-" -> Seq(Vect.LEFT, Vect.RIGHT),
    "L" -> Seq(Vect.UP, Vect.RIGHT),
    "J" -> Seq(Vect.UP, Vect.LEFT),
    "7" -> Seq(Vect.DOWN, Vect.LEFT),
    "F" -> Seq(Vect.DOWN, Vect.RIGHT),
  )

  def readData(): Array[Array[String]] = {
    readLines("day10.txt")
      .map(_.split(""))
  }

  def find(array: Array[Array[String]], str: String): Point = {
    for (i <- array.indices) {
      for (j <- array(0).indices) {
        if (array(i)(j) == "S") {
          return Point(i, j)
        }
      }
    }
    Point(-1, -1)
  }

  def findPath(arr: Array[Array[String]]): Set[Point] = {
    var visited = Set.empty[Point]

    val startingPoint = find(arr, "S")

    def isCorrect(point: Point): Boolean = point.isCorrect(arr.length, arr(0).length)

    // first from starting point
    var nextPoint = connections(arr.at(startingPoint))
      .map(startingPoint.move)
      .filter(isCorrect)
      .filterNot(p => arr.at(p) == ".")
      .filterNot(visited.contains).head
    visited = visited + nextPoint
    // make one more step
    nextPoint = connections(arr.at(nextPoint))
      .map(nextPoint.move)
      .filter(isCorrect)
      .filterNot(visited.contains)
      .filterNot(_ == startingPoint).head
    visited = visited + nextPoint
    // go till the starting point
    while (nextPoint != startingPoint) {
      visited = visited + nextPoint
      nextPoint = connections(arr.at(nextPoint))
        .map(nextPoint.move)
        .filter(isCorrect)
        .filterNot(visited.contains).head
    }
    visited + startingPoint
  }




  def enlarge(arr: Array[Array[String]]): Array[Array[String]] = {
    val mapping = Map(
      "S" -> ".#.###.#.".grouped(3).toSeq,
      "|" -> ".#..#..#.".grouped(3).toSeq,
      "-" -> "...###...".grouped(3).toSeq,
      "L" -> ".#..##...".grouped(3).toSeq,
      "J" -> ".#.##....".grouped(3).toSeq,
      "7" -> "...##..#.".grouped(3).toSeq,
      "F" -> "....##.#.".grouped(3).toSeq,
      "." -> ".........".grouped(3).toSeq
    )
    val arr2 = arr.flatMap(row =>
      row.foldLeft(Array("", "", ""))((acc, curr) => {
        val m = mapping(curr)
        Array(
          acc(0) + m(0).mkString,
          acc(1) + m(1).mkString,
          acc(2) + m(2).mkString
        )
      }
      )
    ).map(_.split(""))
    arr2
  }

  def markPath(arr: Array[Array[String]], visited: Set[Point]): Array[Array[String]] = {
    var res = Array.empty[Array[String]]
    for (i <- arr.indices) {
      var res2 = Array.empty[String]
      for (j <- arr(0).indices) {
        if (visited.contains(Point(i, j)))
          res2 = res2 :+ arr(i)(j)
        else res2 = res2 :+ "."
      }
      res = res :+ res2
    }
    res
  }

  def markVisited(arr: Array[Array[String]], visited: Set[Point]): Array[Array[String]] = {
    var res = Array.empty[Array[String]]
    for (i <- arr.indices) {
      var res2 = Array.empty[String]
      for (j <- arr(0).indices) {
        if (visited.contains(Point(i, j)))
          res2 = res2 :+ "v"
        else res2 = res2 :+ "."
      }
      res = res :+ res2
    }
    res
  }

  def part1(): BigInt = {
    val arr = readData()
    val visited = findPath(arr)
    visited.size / 2 + 1
  }

  def part2(): BigInt = {
    val arr = readData()

    def isCorrect(array: Array[Array[String]])(point: Point): Boolean = point.isCorrect(array.length, array(0).length)

    val visited = findPath(arr)
    val arrWithPath = markPath(arr, visited)
    //arrWithPath.show("arr with path")

    val biggerArr = enlarge(arrWithPath)
    // biggerArr.show("bigger arr")

    val enlargedVisited = visited.flatMap(p => {
      val newPoint = Point((p.x * 3) + 1, (p.y * 3) + 1)
      //println(f"$p , $newPoint ${newPoint.neighborsAdjacent}")
      newPoint.neighborsAdjacent :+ newPoint
    }
    )

    //markVisited(biggerArr, enlargedVisited).show("enlarged visited")

    var reachableFromOutside = Set.empty[Point]

    def getNeighbors(point: Point): Set[Point] = {
      val res = point.neighborsAdjacent
        .filter(isCorrect(biggerArr))
        .filterNot(reachableFromOutside.contains)
        .filterNot(p => biggerArr.at(p) == "#")
        .toSet
      //println(f"p: $point, r: $res")
      res
    }

    val xLen = biggerArr.length
    val yLen = biggerArr(0).length
    val outerPoints = (
      (0 until xLen).zip(Array.fill(xLen)(0))
        ++ (0 until xLen).zip(Array.fill(xLen)(yLen - 1))
        ++ (0 until yLen).zip(Array.fill(yLen)(0)).map(_.swap)
        ++ (0 until yLen).zip(Array.fill(yLen)(xLen - 1)).map(_.swap)
      ).map(el => Point(el._1, el._2))
      .filterNot(enlargedVisited.contains)

    outerPoints.foreach { p =>
      reachableFromOutside = reachableFromOutside ++ floodFill(p, getNeighbors)
    }

    //markVisited(biggerArr, reachableFromOutside).show("reachable from outside")

    val flooded = markVisited(biggerArr, reachableFromOutside ++ enlargedVisited)
    //flooded.show(" fully flooded")
    flooded.map(_.count(_ == ".")).sum / 9
  }

  def main(args: Array[String]): Unit = {
    print(part2())
  }
}
