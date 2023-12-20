package days

import utils._
import utils.Vect._

object Day16 {

  val lines = readAsMatrix("day16.txt")

  given l: M[Char] = lines

  case class Beam(position: Point, direction: Vect)

  def move(b: Beam, beamsMem: collection.mutable.Set[Beam])(using arr: M[Char]): Seq[Beam] = {
    val currentPoint = b.position //.move(b.direction)
    if (!currentPoint.isCorrect) {
      return Seq.empty[Beam]
    }
    val currentTile = arr.at(currentPoint)
    val nextBeams = currentTile match {
      case '|' => b.direction match {
        case LEFT | RIGHT => Seq(Beam(currentPoint.move(UP), UP), Beam(currentPoint.move(DOWN), DOWN))
        case UP | DOWN => Seq(Beam(currentPoint.move(b.direction), b.direction))
      }
      case '-' => b.direction match {
        case LEFT | RIGHT => Seq(Beam(currentPoint.move(b.direction), b.direction))
        case UP | DOWN => Seq(Beam(currentPoint.move(RIGHT), RIGHT), Beam(currentPoint.move(LEFT), LEFT))
      }
      case '\\' => b.direction match {
        case LEFT => Seq(Beam(currentPoint.move(UP), UP))
        case RIGHT => Seq(Beam(currentPoint.move(DOWN), DOWN))
        case UP => Seq(Beam(currentPoint.move(LEFT), LEFT))
        case DOWN => Seq(Beam(currentPoint.move(RIGHT), RIGHT))
      }
      case '/' => b.direction match {
        case LEFT => Seq(Beam(currentPoint.move(DOWN), DOWN))
        case RIGHT => Seq(Beam(currentPoint.move(UP), UP))
        case UP => Seq(Beam(currentPoint.move(RIGHT), RIGHT))
        case DOWN => Seq(Beam(currentPoint.move(LEFT), LEFT))
      }
      case '.' => Seq(Beam(currentPoint.move(b.direction), b.direction))
    }
    val correct = nextBeams
      .filter(b => b.position.isCorrect)
      .filterNot(b => beamsMem.contains(b))
    correct.foreach(c => beamsMem.add(c))
    correct
  }

  def part1(): Long = {
    val startingBeam = Beam(Point(0, 0), RIGHT)
    countEnergized(startingBeam)
  }

  private def countEnergized(startingBeam: Beam) = {
    val beamsMem = collection.mutable.Set.empty[Beam]
    beamsMem.add(startingBeam)
    var beams = Seq(startingBeam)
    while (beams.nonEmpty) {
      beams = beams.flatMap(b => move(b, beamsMem))
    }
    beamsMem.map(_.position).toSet.size
  }

  def part2(): Long = {
    val topBeams = lines.indices.map(y => Beam(Point(0, y), DOWN))
    val bottomBeams = lines.indices.map(y => Beam(Point(lines.head.length - 1, y), UP))
    val leftBeams = lines.head.indices.map(x => Beam(Point(x, 0), RIGHT))
    val rightBeams = lines.head.indices.map(x => Beam(Point(x, lines.length - 1), LEFT))
    (topBeams ++ bottomBeams ++ leftBeams ++ rightBeams)
      .map(countEnergized)
      .max
  }

  def main(args: Array[String]): Unit = {
    check(part1,7798)
    check(part2,8026)
  }
}
