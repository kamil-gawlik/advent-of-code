package days

import utils._

object Day15 {

  val line: String = readLines("day15.txt")(0)


  case class Lens(label: String, focalLength: Int)

  sealed trait Command

  case class Remove(label: String) extends Command

  case class Add(label: String, focalLength: Int) extends Command {
    def toLens: Lens = Lens(label, focalLength)
  }

  object Command {
    private val addPattern = "(.+)=(.+)".r
    private val removePattern = "(.+)-".r

    def of(s: String): Command = s.match {
      case addPattern(label, focalLength) => Add(label, focalLength.toInt)
      case removePattern(label) => Remove(label)
    }
  }


  def hash(s: String): Int = {
    s.foldLeft(0)((curr, char) =>
      val curr2: Int = curr + char.intValue
      val curr3: Int = curr2 * 17
      curr3 % 256
    )
  }

  def part1(): BigInt = {
    val commands = line.split(",")
    commands.map(hash).sum
  }

  val boxes: collection.mutable.Map[Int, Seq[Lens]] = collection.mutable.Map.from((0 to 255).zip(Seq.fill(256)(Seq.empty[Lens])).toMap)
  extension[A] (m: collection.mutable.Map[Int, Seq[A]]) {
    def show(): Unit = {
      m.keySet.toSeq.sorted
        .foreach { k =>
          if (m(k).nonEmpty) {
            println(s"Box $k: [${m(k)}]")
          }
        }
      println()
    }
  }


  def calculateFocusingPower(i: Int, s: Seq[Lens]): BigInt = {
    s.zipWithIndex.map(el =>
      (i + 1) * (el._2 + 1) * el._1.focalLength
    ).sum
  }

  def part2(): BigInt = {
    val commands = line.split(",").map(Command.of)

    def replace[A](s: Seq[A], oldV: A, newV: A): Seq[A] = {
      s.map(e => if (e == oldV) newV else e)
    }

    commands.foreach { c =>
      c match {
        case a: Add => {
          val hashV = hash(a.label)
          val box = boxes(hashV)
          val found = box.find(l => l.label == a.label)
          var newBox: Seq[Lens] = null
          if (found.isDefined) {
            newBox = replace(box, found.get, a.toLens)
          } else {
            newBox = box :+ a.toLens
          }
          boxes.put(hashV, newBox)
        }
        case r: Remove => {
          val hashV = hash(r.label)
          val box = boxes(hashV)
          val newBox = box.filterNot(l => l.label == r.label)
          boxes.put(hashV, newBox)
        }
      }
      //boxes.show()
    }

    boxes.view.map(e => calculateFocusingPower(e._1, e._2)).sum

  }

  def main(args: Array[String]): Unit = timeMs {
    println(part2())
  }
}
