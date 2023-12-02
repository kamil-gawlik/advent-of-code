package days

import utils.Dijkstra.WeightedDiGraph
import utils._

import scala.collection.mutable.ArrayBuffer

object Day12 {
  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  private def part1(): Unit = {
    val arr = readLines("day12.txt")
      .map(_.split("").map { c =>
        if (c.charAt(0) == 'S') {
          'a' - 1
        } else if (c.charAt(0) == 'E') {
          'z' + 1
        } else {
          c.charAt(0).toInt
        }
      })

    val roadMap = new WeightedDiGraph[Vertex]

    def getIfValid(v: Vertex): Option[Int] = if (v.x >= 0 && v.y >= 0 && v.x < arr.length && v.y < arr(0).length) Some(arr(v.x)(v.y)) else None

    var start: Vertex = null
    var end: Vertex = null
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        var vertexValue = arr(i)(j)
        if (vertexValue == 'a' - 1) {
          start = Vertex(i, j)
        }
        if (vertexValue == 'z' + 1) {
          end = Vertex(i, j)
        }
        List((-1, 0), (1, 0), (0, -1), (0, 1)).foreach { z =>
          val to = Vertex(i + z._1, j + z._2)
          getIfValid(to).map { value =>
            val diff = (-1 * (vertexValue - value))
            if (diff <= 1) {
              println(s"${Vertex(i, j)} $to $value $vertexValue $diff")
              roadMap.addArc(Vertex(i, j), to, diff + 100) // normalize values so not weight is lower than zero
            }
          }
        }
      }
    }
    val res = roadMap.shortestPath(start, end)
    res.foreach(println)
    println(res.length)
  }

  private def part2(): Unit = {
    val arr = readLines("day12.txt")
      .map(_.split("").map { c =>
        if (c.charAt(0) == 'S') {
          'a' - 1
        } else if (c.charAt(0) == 'E') {
          'z' + 1
        } else {
          c.charAt(0).toInt
        }
      })

    val roadMap = new WeightedDiGraph[Vertex]

    def getIfValid(v: Vertex): Option[Int] = if (v.x >= 0 && v.y >= 0 && v.x < arr.length && v.y < arr(0).length) Some(arr(v.x)(v.y)) else None

    var start: Vertex = null
    var end: Vertex = null
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        var vertexValue = arr(i)(j)
        if (vertexValue == 'a' - 1) {
          start = Vertex(i, j)
        }
        if (vertexValue == 'z' + 1) {
          end = Vertex(i, j)
        }
        List((-1, 0), (1, 0), (0, -1), (0, 1)).foreach { z =>
          val to = Vertex(i + z._1, j + z._2)
          getIfValid(to).map { value =>
            val diff = (-1 * (vertexValue - value))
            if (diff <= 1) {
              println(s"${Vertex(i, j)} $to $value $vertexValue $diff")
              roadMap.addArc(Vertex(i, j), to, diff + 100) // normalize values so not weight is lower than zero
            }
          }
        }
      }
    }
    val pathDistances = ArrayBuffer.empty[Int]
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        if (arr(i)(j) <= 'a') {
          pathDistances.addOne(roadMap.shortestPath(Vertex(i, j), end).length)
        }
      }
    }
    println(pathDistances.filter(_ != 0).min)
  }

  case class Vertex(x: Int, y: Int)

}
