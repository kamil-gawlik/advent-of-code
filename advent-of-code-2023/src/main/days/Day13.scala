package days

import utils._

object Day13 {

  case class Pattern(arr: Seq[String])

  def readData(): Seq[Pattern] = {
    readLinesSplitEmptyLine("day13.txt")
      .map(a => Pattern(a))
  }


  def findVerticalMirror(p: Pattern): Int = {
    findMirror(p.arr)
  }

  def findVerticalMirrorWithSmudge(p: Pattern): Int = {
    findMirrorWithSmudge(p.arr)
  }

  def findHorrizontalMirror(p: Pattern): Int = {
    findMirror(p.arr.transpose.map(_.mkString)) * 100
  }

  def findHorrizontalMirrorWithSmudge(p: Pattern): Int = {
    findMirrorWithSmudge(p.arr.transpose.map(_.mkString)) * 100
  }

  private def findMirror(arr: Seq[String]): Int = {
    val firstLine = arr.head
    val restOfLines = arr.tail

    def checkLine(line: String, splitAt: Int, mirrorSize: Int): Boolean = {
      val left = line.slice(splitAt - mirrorSize, splitAt)
      val right = line.slice(splitAt, splitAt + mirrorSize)
      //println(f"$left $right ($mirrorSize) s: $splitAt")
      left == right.reverse
    }

    for (i <- Range(1, firstLine.length)) {
      val mirrorSize = Math.min(i, firstLine.length - i)
      if (checkLine(firstLine, i, mirrorSize)) {
        if (restOfLines.map(r => checkLine(r, i, mirrorSize)).reduce(_ & _)) {
          return i
        }
      }
    }
    0
  }

  private def findMirrorWithSmudge(arr: Seq[String]): Int = {
    val firstLine = arr.head

    def checkLineWithSmudge(line: String, splitAt: Int, mirrorSize: Int): Int = {
      val left = line.slice(splitAt - mirrorSize, splitAt)
      val right = line.slice(splitAt, splitAt + mirrorSize)
      val differences = left.reverse.zip(right).count(el => el._1 != el._2)
      //println(f"$left $right ($mirrorSize) s: $splitAt, diff: $differences")
      differences
    }

    for (i <- Range(1, firstLine.length)) {
      val mirrorSize = Math.min(i, firstLine.length - i)
      val changesNeeded = arr.map(l => checkLineWithSmudge(l, i, mirrorSize)).sum
      //println(f"($mirrorSize) $i -> $changesNeeded")
      if (changesNeeded == 1) {
        return i
      }
    }
    0
  }

  def part1(): Long = {
    val patterns = readData()
    val vertical = patterns.map(findVerticalMirror).sum
    val horizontal = patterns.map(findHorrizontalMirror).sum
    vertical + horizontal
  }

  def part2(): Long = {
    val patterns = readData()
    val vertical = patterns.map(findVerticalMirrorWithSmudge).sum
    val horizontal = patterns.map(findHorrizontalMirrorWithSmudge).sum
    vertical + horizontal
  }

  def main(args: Array[String]): Unit = {
    check(part1,27505)
    check(part2,22906)
  }
}
