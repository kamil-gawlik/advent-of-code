package days

import utils._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Day08 {
  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }

  private def part1(): Unit = {
    var counter: Int = 0
    val arr = readInput()

    def isVisible(x: Int, y: Int): Boolean = {
      // using stepF accepting tuple to compose function in recursive call
      def recursiveVisibilityCheck(x: Int, y: Int)(stepF: ((Int, Int)) => (Int, Int)): Boolean = {
        val baseStepFunction = stepF
        val checked = arr(x)(y)

        def tailRecF(internalStepF: ((Int, Int)) => (Int, Int)): Boolean = {
          val (compared_x, compared_y) = internalStepF((x, y))
          //println(s"comparing [$x,$y], [$compared_x,$compared_y]")
          if (compared_x >= 0 && compared_y >= 0 && compared_x < arr.length && compared_y < arr(0).length) {
            //println(s"$checked [$x,$y] vs ${arr(compared_x)(compared_y)} [$compared_x,$compared_y]")
            checked > arr(compared_x)(compared_y) && tailRecF(internalStepF.compose(baseStepFunction)) // make recursive, deeper step
          } else true
        }

        tailRecF(baseStepFunction)
      }

      val checkFor = recursiveVisibilityCheck(x, y) _
      val res = (
        checkFor((in: (Int, Int)) => (in._1 - 1, in._2)) ||
          checkFor((in: (Int, Int)) => (in._1 + 1, in._2)) ||
          checkFor((in: (Int, Int)) => (in._1, in._2 - 1)) ||
          checkFor((in: (Int, Int)) => (in._1, in._2 + 1))
        )
      //println(s"res for [$x,$y] is $res")
      res
    }

    for (i <- 1 until arr.length - 1) { // we are not counting trees on edge
      for (j <- 1 until arr(0).length - 1) {
        if (isVisible(i, j)) {
          counter = counter + 1
          print("V")
        } else {
          print("n")
        }
      }
      print("\n")
    }
    val edge = arr.length * 2 + arr(0).length * 2 - 4
    println(counter + edge)
  }

  private def part2(): Unit = {
    val arr = readInput()

    def calculateScore(x: Int, y: Int): Int = {
      // using stepF accepting tuple to compose function in recursive call
      def recursiveScoreCounter(x: Int, y: Int)(stepF: ((Int, Int)) => (Int, Int)): Int = {
        val baseStepFunction = stepF
        val checked = arr(x)(y)

        def tailRecF(internalStepF: ((Int, Int)) => (Int, Int)): Int = {
          val (compared_x, compared_y) = internalStepF((x, y))
          //println(s"comparing [$x,$y], [$compared_x,$compared_y]")
          if (compared_x >= 0 && compared_y >= 0 && compared_x < arr.length && compared_y < arr(0).length) {
            //println(s"$checked [$x,$y] vs ${arr(compared_x)(compared_y)} [$compared_x,$compared_y]")
            if (checked > arr(compared_x)(compared_y)) {
              1 + tailRecF(internalStepF.compose(baseStepFunction)) // make recursive, deeper step
            } else {
              1
            }
          } else 0
        }

        tailRecF(baseStepFunction)
      }

      val checkFor = recursiveScoreCounter(x, y) _
      val res = (
        checkFor((in: (Int, Int)) => (in._1 - 1, in._2)) *
          checkFor((in: (Int, Int)) => (in._1 + 1, in._2)) *
          checkFor((in: (Int, Int)) => (in._1, in._2 - 1)) *
          checkFor((in: (Int, Int)) => (in._1, in._2 + 1))
        )
      //println(s"res for [$x,$y] is $res")
      res
    }

    val scores = ArrayBuffer[Int]()
    for (i <- arr.indices) {
      for (j <- arr(0).indices) {
        val tmpRes = calculateScore(i, j)
        print(s"$tmpRes\t")
        scores.addOne(tmpRes)
      }
      println()
    }
    println()
    println(scores.max)
  }


  private def readInput(): Array[Array[Int]] = {
    readLines("day08.txt")
      .map(_.split("").map(_.toInt))
  }
}
