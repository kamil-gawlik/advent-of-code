package days

import utils._

object Day14 {

  private val lines: M[Char] = readAsMatrix("day14.txt")

  def tiltLeft(in: Seq[Char]): Seq[Char] = {
    val s = in.mkString

    def tiltSubstring(s: String): String = {
      val oCount = s.count(_ == 'O')
      val emptyCount = s.count(_ == '.')
      "O" * oCount + "." * emptyCount
    }

    def insertHash(s: String, pos: Int): String = {
      val first = s.substring(0, pos)
      val last = s.substring(pos, s.length)
      first + "#" + last
    }

    val hashPositions = s.zipWithIndex.filter(el => el._1 == '#').map(_._2)
    val tiltedSubstrings = s.split('#').map(tiltSubstring).mkString
    val res = hashPositions.foldLeft(tiltedSubstrings)((acc, pos) => insertHash(acc, pos)).toCharArray
    res
  }

  def tiltNorth(in: M[Char]): M[Char] = {
    val horizontalPresentation = in.transpose.map(tiltLeft)
    val res = horizontalPresentation.transpose
    res
  }

  def cycle(in: M[Char]): M[Char] = {
    val n = in.transpose.map(tiltLeft).transpose
    val w = n.map(tiltLeft)
    val s = w.transpose.map(_.reverse).map(tiltLeft).map(_.reverse).transpose
    val e = s.map(_.reverse).map(tiltLeft).map(_.reverse)
    e
  }

  def calculateNorthWeight(c: M[Char]): BigInt = {
    c.reverse // index is now naturally aligned with north weight
      .zipWithIndex
      .map(el => el._1.count(_ == 'O') * (el._2 + 1))
      .sum
  }

  def part1(): BigInt = {
    val tilted = tiltNorth(lines)
    calculateNorthWeight(tilted)
  }

  def part2(): BigInt = {
    var l = lines
    val targetIterations: BigInt = 1000000000
    val cycleMemo = collection.mutable.Map.empty[Int, BigInt]

    var i = BigInt(0)
    while (!cycleMemo.contains(l.hashCode())) {
      cycleMemo.put(l.hashCode(), i)
      l = cycle(l)
      i = i + 1
    }
    val repeatStart = cycleMemo(l.hashCode())
    val repeatSize = i - repeatStart
    (BigInt(0) until (targetIterations - repeatStart) % repeatSize).foreach { _ =>
      l = cycle(l)
    }
    calculateNorthWeight(l)
  }

  def main(args: Array[String]): Unit = timeMs {
    println(part2())
  }
}
