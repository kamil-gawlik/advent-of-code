package days

import utils._

object Day21 {
  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  private def part1(): Unit = {
    val opsMap = readOperations()
    println(calculateValue("root", opsMap))
  }

  private def part2(): Unit = {
    val opsMap = readOperations()
    println(calculateValue("humn", reverseOps("humn", opsMap)))
  }

  private val value_p = """(\d+)""".r
  private val add_p = """(.+) \+ (.+)""".r
  private val minus_p = """(.+) \- (.+)""".r
  private val div_p = """(.+) \/ (.+)""".r
  private val mul_p = """(.+) \* (.+)""".r

  def calculateValue(id: String, opMap: Map[String, String]): Long = {
    id.toLongOption match {
      case Some(num) => num
      case None => opMap(id) match {
        case value_p(v) => v.toLong
        case add_p(v1, v2) => calculateValue(v1, opMap) + calculateValue(v2, opMap)
        case minus_p(v1, v2) => calculateValue(v1, opMap) - calculateValue(v2, opMap)
        case div_p(v1, v2) => calculateValue(v1, opMap) / calculateValue(v2, opMap)
        case mul_p(v1, v2) => calculateValue(v1, opMap) * calculateValue(v2, opMap)
      }
    }
  }

  def reverseOps(id: String, opsMap: Map[String, String]): Map[String, String] = {
    def findNotSearched(s1: String, s2: String): String = if (s1 == id) s2 else s1

    def isSearched(s1: String) = if (s1 == id) true else false

    val searchedCalc = opsMap.find { case (k, v) => v.contains(id) }
    val prevParent = searchedCalc.get._1
    val newOp = searchedCalc.get._2 match {
      case add_p(v1, v2) => s"$prevParent - ${findNotSearched(v1, v2)}"
      case minus_p(v1, v2) => if (isSearched(v1)) s"$prevParent + $v2" else s"$v1 - $prevParent"
      case div_p(v1, v2) => if (isSearched(v1)) s"$prevParent * $v2" else s"$v1 / $prevParent"
      case mul_p(v1, v2) => s"$prevParent / ${findNotSearched(v1, v2)}"
    }

    if (newOp.contains("root")) {
      val lookingFor = newOp.split("""[+-/*]""").map(_.trim).filterNot(_ == "root")(0)
      opsMap - prevParent + (id -> calculateValue(lookingFor, opsMap).toString)
    } else {
      reverseOps(prevParent, opsMap - prevParent) + (id -> newOp)
    }
  }
  
  def readOperations(): Map[String, String] = {
    readLines("day21.txt")
      .map {
        l =>
          val ll = l.split(":")
          val (id, op) = (ll(0), ll(1).trim)
          id -> op
      }.toMap
  }


}
