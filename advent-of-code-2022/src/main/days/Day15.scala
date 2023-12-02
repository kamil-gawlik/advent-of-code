package days

import utils._

import scala.annotation.tailrec


object Day15 {
  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  private def part1(): Unit = {
    val sensors = readLines("day15.txt")
      .map(Sensor.of)
    val xDimensions = sensors.flatMap(s => Array(s.position.x, s.closestBeacon.x))
    val takenPositions = sensors.flatMap(s => Array(s.position, s.closestBeacon)).toSet
    val maxDistance = sensors.map(s => s.closestBeacon.manhatanDist(s.position)).max

    val coveredPoints = (xDimensions.min - maxDistance to xDimensions.max + maxDistance).map { x =>
      val p = Point(x, 2000000)
      if (sensors.exists(isCovered(p, _))) {
        Some(p)
      } else {
        None
      }
    }.filter(_.nonEmpty).map(_.get)
      .filter(!takenPositions.contains(_))
    println(coveredPoints.size)
  }

  private def part2(): Unit = {
    val sensors = readLines("day15.txt")
      .map(Sensor.of)

    @tailrec
    def helper(sensors: Seq[Sensor], y: Int): Long = {
      intervalsForY(sensors, y) match {
        case Seq(first, second) => {
          val x = Math.min(first.max, second.max) + 1
          println(s"$x $y")
          4000000L * x + y
        }
        case _ => helper(sensors, y + 1)
      }
    }

    println(helper(sensors, 0))
  }

  def isCovered(p: Point, s: Sensor): Boolean = {
    val distToClosest = s.closestBeacon.manhatanDist(s.position)
    val pointDist = p.manhatanDist(s.position)
    (pointDist <= distToClosest)
  }

  def intervalsForY(sensors: Seq[Sensor], y: Int): Seq[Interval] = {
    sensors.foldLeft(Seq.empty[Interval]) { case (intervals, s) =>
      val takenFromRow = s.manhatan - (s.position.y - y).abs
      if (takenFromRow < 0) {
        intervals
      } else {
        val newInterval = Interval(s.position.x - takenFromRow, s.position.x + takenFromRow)
        val (toBeMerged, notConnected) = intervals.partition(_.touches(newInterval))
        toBeMerged.foldLeft(newInterval)(_ merge _) +: notConnected
      }
    }
  }

  case class Interval(min: Int, max: Int) {
    def merge(other: Interval): Interval = Interval(Math.min(min, other.min), Math.max(max, other.max))

    def touches(other: Interval): Boolean = if (other.min < min) min - other.max <= 1 else other.min - max <= 1
  }

  implicit class PositionOps(p: Point) {
    def manhatanDist(other: Point): Int = Math.abs(other.x - p.x) + Math.abs(other.y - p.y)
  }

  case class Sensor(position: Point, closestBeacon: Point) {
    def manhatan: Int = position.manhatanDist(closestBeacon)
  }

  object Sensor {
    private val pattern = """Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)""".r

    def of(s: String): Sensor = s match {
      case pattern(x, y, bx, by) => Sensor(Point(x.toInt, y.toInt), Point(bx.toInt, by.toInt))
    }
  }
}
