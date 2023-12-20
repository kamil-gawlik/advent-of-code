package days

import utils.*
import utils.Vect.*


object Day17 {

  given m: M[Int] = readAsMatrix("day17.txt").map(_.map(_.toString.toInt))

  type PathKey = (Point, Vect, Int)

  case class Path(point: Point, dir: Vect, cost: Int, consecutiveDir: Int) {
    def getKey: PathKey = (point, dir, consecutiveDir)
  }

  // all but reverse dirs
  private val possibleDirections = Map(
    UP -> Seq(UP, RIGHT, LEFT),
    LEFT -> Seq(DOWN, LEFT, UP),
    RIGHT -> Seq(DOWN, UP, RIGHT),
    DOWN -> Seq(DOWN, RIGHT, LEFT),
  )

  def getNeighbors(p: Path, minConsecutive: Int, maxConsecutive: Int): Set[Path] = {
    var res = Set.empty[Path]
    if (p.consecutiveDir < minConsecutive) {
      val nextPoint = p.point.move(p.dir)
      if (nextPoint.isCorrect) {
        val nextCost = p.cost + m.at(nextPoint)
        res = res + Path(nextPoint, p.dir, nextCost, p.consecutiveDir + 1)
      }
    } else {
      possibleDirections(p.dir).foreach { nextDirection =>
        val nextPoint = p.point.move(nextDirection)
        val nextConsDir = if (p.dir == nextDirection) p.consecutiveDir + 1 else 1
        if (nextPoint.isCorrect && nextConsDir <= maxConsecutive) {
          val nextCost = p.cost + m.at(nextPoint)
          res = res + Path(nextPoint, nextDirection, nextCost, nextConsDir)
        }
      }
    }
    res
  }

  def findPath(start: Point, end: Point, minConsecutive: Int, maxConsecutive: Int): Long = {
    var queue = Set.empty[Path]
    var costMap = Map.empty[PathKey, Int]

        val point01 = Path(Point(0, 1), RIGHT, m.at(Point(0, 1)), 1)
        val point10 = Path(Point(1, 0), DOWN, m.at(Point(1, 0)), 1)
        val startPath = Seq(point01, point10)
        queue = queue ++ startPath
        costMap = costMap + (point10.getKey -> point01.cost)
        costMap = costMap + (point01.getKey -> point01.cost)

/*
    val startPoint = Path(Point(0, 0), DOWN, 0, 0)
    queue = queue + startPoint
    costMap = costMap + (startPoint.getKey -> 0)
*/

    while (queue.nonEmpty) {
      val curr = queue.minBy(_.cost)
      if (curr.point == end && !(curr.consecutiveDir < minConsecutive)) {
        return curr.cost
      }
      queue = queue.filterNot(_ == curr)
      getNeighbors(curr, minConsecutive, maxConsecutive).foreach { n =>
        val currMinCost = costMap.get(n.getKey)
        if (currMinCost.isEmpty || currMinCost.get > n.cost) {
          costMap = costMap + (n.getKey -> n.cost)
          queue = queue + n
        }
      }
    }
    1
  }


  def part1(): Long = {
    val startingPoint = Point(0, 0)
    val end = Point(m.length - 1, m.head.length - 1)
    findPath(startingPoint, end, 0, 3)
  }

  def part2(): Long = {
    val startingPoint = Point(0, 0)
    val end = Point(m.length - 1, m.head.length - 1)
    findPath(startingPoint, end, 4, 10)
  }


  def main(args: Array[String]): Unit = {
    check(part1, 665)
    check(part2, 809)
  }
}
