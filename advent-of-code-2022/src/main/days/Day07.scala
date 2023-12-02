package days

import utils._

import scala.collection.mutable
import scala.util.matching.Regex

object Day07 {

  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  def part1(): Unit = {
    def getDirsSmallerThan(dir: Dir, maxSize: Int = 100001): List[Dir] = {
      val buff = mutable.ArrayBuffer[Dir]()

      def add(d: Dir): Unit = {
        if (d.size < maxSize) {
          buff.addOne(d)
        }
        d.dirs.foreach(add)
      }

      add(dir)
      buff.toList
    }

    val structure = readFileSystem("day07.txt")
    println(getDirsSmallerThan(structure)
      .map(_.size)
      .sum)
  }

  def part2(): Unit = {
    def getDirsBiggerThan(dir: Dir, minSize: Int): List[Dir] = {
      val buff = mutable.ArrayBuffer[Dir]()

      def add(d: Dir): Unit = {
        if (d.size > minSize) {
          buff.addOne(d)
        }
        d.dirs.foreach(add)
      }

      add(dir)
      buff.toList
    }

    val structure = readFileSystem("day07.txt")
    val freeSpace = 70000000 - structure.size
    val missingSpace = 30000000 - freeSpace
    println(getDirsBiggerThan(structure, missingSpace)
      .map(_.size)
      .filter(_ >= missingSpace)
      .min
    )
  }

  private def readFileSystem(filename: String): Dir = {
    val lines = readLines("day07.txt")
    val rootDir = Dir(null, "/")
    var pointer: Dir = rootDir
    import Commands._
    lines.foreach {
      case changeDirPattern(dir) => dir match {
        case "/" => {
          println("going to /")
          pointer = rootDir
        }
        case ".." => {
          println(s"going to ${pointer.parent.name}")
          pointer = pointer.parent
        }
        case _ => pointer = {
          println(s"going to $dir")
          pointer.change(dir)
        }
      }
      case listPattern(_) => {
        println(s"listing dir")
      }
      case dirPattern(dir) => {
        println(s"dir $dir")
        pointer.addDir(dir)
      }
      case filePattern(size, name) => {
        println(s"file ${name} with size $size")
        pointer.addFile(name, size.toInt)
      }
      case _ => println("unknown command")
    }
    rootDir
  }

  object Commands {
    val changeDirPattern: Regex = "^\\$ cd (.+)$".r
    val listPattern: Regex = "^\\$ (ls)$".r
    val dirPattern: Regex = "^dir (.+)$".r
    val filePattern: Regex = "^(\\d+) (.+)$".r
  }

  trait Node {
    def size: Int

    def name: String
  }

  case class Dir(parent: Dir, name: String, dirs: mutable.ArrayBuffer[Dir], files: mutable.ArrayBuffer[File]) extends Node {
    override def toString(): String = {
      val fString = files.foldLeft(new mutable.StringBuilder())((sb, f) => sb.append(s" f[name: ${f.name}, size: ${f.size}]\n"))
      val dString = dirs.foldLeft(new StringBuilder())((sb, d) => sb.append(d.toString()))
      s"""d[name: $name, size:$size]
         |$fString
         |$dString
      """.stripMargin
    }

    def change(name: String): Dir = {
      dirs.find {
        case Dir(_, n, _, _) if n == name => true
        case _ => false
      }.get
    }

    def addDir(name: String): Unit = {
      val d = dirs.find {
        case Dir(_, n, _, _) if n == name => true
        case _ => false
      }
      if (d.isEmpty) {
        val newDir = Dir(this, name)
        this.dirs.addOne(newDir)
      }
    }

    def addFile(name: String, size: Int): Unit = {
      val f = files.find {
        case File(_, _, n) if n == name => true
        case _ => false
      }
      if (f.isEmpty) {
        val newFile = File(this, size, name)
        this.files.addOne(newFile)
      }
    }

    def getParent: Dir = parent

    override def size: Int = dirs.map(_.size).sum + files.map(_.size).sum
  }

  object Dir {
    def apply(parent: Dir, name: String): Dir = Dir(parent, name, mutable.ArrayBuffer.empty, mutable.ArrayBuffer.empty)
  }

  case class File(parent: Dir, size: Int, name: String) extends Node

}
