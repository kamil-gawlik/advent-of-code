package days

import utils._

object Day18 {
  def main(args: Array[String]): Unit = {
    part2()
  }

  private def part1(): Unit = {
    val cubes = readCubes()
    val res: Int = cubesSurface(cubes)
    println(res)
  }

  private def part2(): Unit = {
    val cubes = readCubes()
    val minX = cubes.map(_.x).min - 1
    val minY = cubes.map(_.y).min - 1
    val minZ = cubes.map(_.z).min - 1
    val maxX = cubes.map(_.x).max + 1
    val maxY = cubes.map(_.y).max + 1
    val maxZ = cubes.map(_.z).max + 1

    def getNeighbors(cube: Cube): Set[Cube] = {
      val (x, y, z) = (cube.x, cube.y, cube.z)
      Set(
        (x + 1, y, z),
        (x - 1, y, z),
        (x, y + 1, z),
        (x, y - 1, z),
        (x, y, z + 1),
        (x, y, z - 1)
      ).filter {
        case (x, y, z) => x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ && !cubes.contains(Cube(x, y, z))
      }.map(Cube.of)
    }

    val externalCubes = floodFill(Cube(minX, minY, minZ), getNeighbors)
    val result = cubes.map { case Cube(x, y, z) =>
      Set(
        Cube(x + 1, y, z),
        Cube(x - 1, y, z),
        Cube(x, y + 1, z),
        Cube(x, y - 1, z),
        Cube(x, y, z + 1),
        Cube(x, y, z - 1)
      ).count(externalCubes.contains)
    }.sum
    println(result)
  }
  
  private def cubesSurface(cubes: Seq[Cube]): Int = {
    val maxSize = cubes.size * 6
    val connectedByX = cubes.combinations(2).filter(seq => seq(0).z == seq(1).z && seq(0).y == seq(1).y).filter(seq => Math.abs(seq(0).x - seq(1).x) == 1).toList
    val connectedByY = cubes.combinations(2).filter(seq => seq(0).x == seq(1).x && seq(0).z == seq(1).z).filter(seq => Math.abs(seq(0).y - seq(1).y) == 1).toList
    val connectedByZ = cubes.combinations(2).filter(seq => seq(0).x == seq(1).x && seq(0).y == seq(1).y).filter(seq => Math.abs(seq(0).z - seq(1).z) == 1).toList
    val res = maxSize - (connectedByX.length + connectedByZ.length + connectedByY.length) * 2
    res
  }

  private def readCubes(): Seq[Cube] = readLines("day18.txt").map(Cube.of)

  case class Diff(ending: Int, size: Int)

  case class Cube(x: Int, y: Int, z: Int)

  object Cube {
    def of(s: String): Cube = {
      val arr = s.split(",")
      Cube(arr(0).toInt, arr(1).toInt, arr(2).toInt)
    }

    def of(t: (Int, Int, Int)): Cube = Cube(t._1, t._2, t._3)
  }
}
