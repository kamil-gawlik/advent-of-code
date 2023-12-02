package days

import utils._

import java.math.{BigInteger, MathContext, RoundingMode}
import scala.collection.mutable.ArrayBuffer

object Day11 {
  def main(args: Array[String]): Unit = {
    //part1()
    part2()
  }


  private def part1(): Unit = {
    val lines = readLinesSplitEmptyLine("day11.txt")
      .map(_.mkString("\n"))
    val monkeys = decodeMonkeys(lines)
    (0 until 20) foreach { _ =>
      monkeys.foreach { m =>
        m.items.foreach { _ =>
          m.increaseCounter()
          val item = m.popItem
          val (throwsTo, newWorryLevel) = m.willThrowTo(item)
          println(s"throws $item with new level $newWorryLevel to $throwsTo")
          monkeys(throwsTo).acceptItem(newWorryLevel)
        }
      }
    }
    val res = monkeys
      .map(_.counter)
      .sorted.reverse
      .take(2)
    println(res(0) * res(1))
  }

  private def part2(): Unit = {
    val lines = readLinesSplitEmptyLine("day11.txt")
      .map(_.mkString("\n"))
    val monkeys = decodeMonkeys(lines)
    val lcm = monkeys.map(_.testDivisibleBy).product
    (0 until 10000) foreach { c =>
      println(c)
      monkeys.foreach { m =>
        m.items.foreach { _ =>
          m.increaseCounter()
          val item = m.popItem
          val (throwsTo, newWorryLevel) = m.willThrowTo(item, 1, lcm)
          //println(s"throws $item with new level $newWorryLevel to $throwsTo")
          monkeys(throwsTo).acceptItem(newWorryLevel)
        }
      }
    }
    monkeys
      .map(_.counter)
      .map(println)
    val res = monkeys
      .map(_.counter.toLong)
      .sorted.reverse
      .take(2)
    println(res(0) * res(1))
  }

  def decodeMonkeys(lines: Array[String]): Array[Monkey] = {
    val pattern =
      """Monkey .+:
        |  Starting items: (.+)
        |  Operation: new = old (.) (.+)
        |  Test: divisible by (.+)
        |    If true: throw to monkey (.+)
        |    If false: throw to monkey (.+)""".stripMargin.r

    lines map {
      case pattern(startingItems, operationType, operationValue, testDivisibleBy, ifTrueThrowTo, ifFalseThrowTo) => {
        val items = startingItems.split(", ").map(i => BigInt(i)) //, new MathContext(0, RoundingMode.HALF_DOWN)), new MathContext(0, RoundingMode.HALF_DOWN)))
        val operation: BigInt => BigInt = operationType match {
          case "*" => operationValue match {
            case "old" => (x: BigInt) => x.pow(2)
            case _ => (x: BigInt) => x * BigInt(operationValue)
          }
          case "+" => (x: BigInt) => x + BigInt(operationValue)
          case "-" => _ - BigInt(operationValue)
          case "/" => _ / BigInt(operationValue)
        }
        val test: BigInt => Boolean = (x: BigInt) => {
          (x % BigInt(testDivisibleBy)) == 0
        }
        val throwsTo: Boolean => Int = (testResult: Boolean) => if (testResult) ifTrueThrowTo.toInt else ifFalseThrowTo.toInt
        Monkey(ArrayBuffer.from(items), operation, test, testDivisibleBy.toLong, throwsTo)
      }
    }
  }

  case class Monkey(var items: ArrayBuffer[BigInt], operation: BigInt => BigInt, test: BigInt => Boolean, testDivisibleBy: Long, throwsTo: Boolean => Int) {
    var counter = 0

    def increaseCounter(): Unit = counter = counter + 1

    def willThrowTo(worryLevel: BigInt, boredMonkeyFactor: BigInt = 3, lcm: Long = 1): (Int, BigInt) = {
      val newWorryLevel = (operation(worryLevel)) % lcm /// boredMonkeyFactor) //.setScale(0, BigInt.RoundingMode.HALF_DOWN)
      (throwsTo(test(newWorryLevel)), newWorryLevel)
    }

    def acceptItem(worryLevel: BigInt): Unit = items.addOne(worryLevel)

    def popItem: BigInt = {
      val item = items.head
      items = items.drop(1)
      item
    }
  }
}
