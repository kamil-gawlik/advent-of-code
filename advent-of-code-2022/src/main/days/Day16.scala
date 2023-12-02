package days

import utils._

import scala.collection.mutable

object Day16 {

  def main(args: Array[String]): Unit = {
    part2()
  }

  private def part1(): Unit = {
    val valves = readLines("day16.txt")
      .map(Valve.of)
    val valvesMap = valves.map(v => v.id -> v).toMap

    def leadsToF: Valve => Seq[Valve] = (v: Valve) => v.leadsTo.map(valvesMap(_))

    val memoized = mutable.HashMap.empty[State, Seq[State]]

    def nextSteps(q: State): Seq[State] = memoized.getOrElse(q, {
      val res = /*if (q.opened.size == valves.length) {
        Seq(q)
      } else */ if (q.remainingTime > 0) {
        leadsToF(q.node).flatMap { nextStep =>
          val notOpened = State(nextStep, remainingTime = q.remainingTime - 1, flowSoFar = q.flowSoFar, opened = q.opened + q.node)
          val openedV =
            if (!q.opened.contains(nextStep)) {
              Seq(State(nextStep, remainingTime = q.remainingTime - 2, flowSoFar = q.flowSoFar + ((q.remainingTime - 2) * nextStep.rate), opened = q.opened + q.node))
            } else {
              Seq.empty
            }
          openedV :+ notOpened
        }
      } else {
        Seq(q)
      }
      //memoized += (q) -> res
      res
    })

    var remainingTime = 30
    var states = Set(State(valvesMap("AA"), remainingTime, 0, Set.empty))
    while (remainingTime > 0) {
      remainingTime = remainingTime - 1
      val (future, current) = states
        .partition(_.remainingTime == remainingTime)
      states = (future ++ current.flatMap(nextSteps)).toList.sortWith(_.flowSoFar > _.flowSoFar).take(100000).toSet


      println(s"$remainingTime")
      //states.toList.sortBy(_.remainingTime).foreach(println)
      println()
    }

    val res = states
      .filter(_.remainingTime == 0)
      .map(_.flowSoFar)
      .max
    println(res)
  }

  private def part2(): Unit = {
    val valves = readLines("day16.txt")
      .map(Valve.of)
    val valvesMap = valves.map(v => v.id -> v).toMap

    def leadsToF: Valve => Seq[Valve] = (v: Valve) => v.leadsTo.map(valvesMap(_))

    def nextSteps(q: State2): Seq[State2] = {
      val res = if (q.remainingTime1 > 0 || q.remainingTime2 > 0) {
        (for {
          n1 <- leadsToF(q.node1)
          n2 <- leadsToF(q.node2) //if n2 != n1
        } yield (n1, n2)).flatMap { case (nextStep1, nextStep2) =>
          val noAction = State2(nextStep1, nextStep2, remainingTime1 = q.remainingTime1 - 1, remainingTime2 = q.remainingTime2 - 1, flowSoFar = q.flowSoFar, opened = q.opened :+ q.node1 :+ q.node2)
          val n1ActionN2noAction = if (!q.opened.contains(nextStep1)) {
            Seq(State2(nextStep1, nextStep2, remainingTime1 = q.remainingTime1 - 2, remainingTime2 = q.remainingTime2 - 1, flowSoFar = q.flowSoFar + ((q.remainingTime1 - 2) * nextStep1.rate), opened = (q.opened :+ q.node1 :+ q.node2)))
          } else {
            Seq.empty
          }
          val n1NoactionN2action = if (!q.opened.contains(nextStep2)) {
            Seq(State2(nextStep1, nextStep2, remainingTime1 = q.remainingTime1 - 1, remainingTime2 = q.remainingTime2 - 2, flowSoFar = q.flowSoFar + ((q.remainingTime2 - 2) * nextStep2.rate), opened = (q.opened :+ q.node1 :+ q.node2)))
          } else {
            Seq.empty
          }
          val n1ActionN2action = if (!q.opened.contains(nextStep1) && !q.opened.contains(nextStep2) && nextStep1 != nextStep2) {
            Seq(State2(nextStep1, nextStep2, remainingTime1 = q.remainingTime1 - 2, remainingTime2 = q.remainingTime2 - 2, flowSoFar = q.flowSoFar + ((q.remainingTime1 - 2) * nextStep1.rate) + ((q.remainingTime2 - 2) * nextStep2.rate), opened = (q.opened :+ q.node1 :+ q.node2)))
          } else {
            Seq.empty
          }
          n1ActionN2noAction ++ n1NoactionN2action ++ n1ActionN2action :+ noAction
        }
      } else {
        Seq(q)
      }
      //memoized += (q) -> res
      res
    }

    var remainingTime = 26
    var states = Set(State2(valvesMap("AA"), valvesMap("AA"), remainingTime, remainingTime, 0, Seq.empty))
    while (remainingTime > 0) {
      remainingTime = remainingTime - 1
      val (future, current) = states
        .partition(x => x.remainingTime1 == remainingTime || x.remainingTime2 == remainingTime)
      states = (future ++ current.flatMap(nextSteps)).toList
        .filterNot(x => x.remainingTime1 < 0 || x.remainingTime2 < 0)
        .sortWith(_.flowSoFar > _.flowSoFar).take(100000).toSet


      println(s"$remainingTime size[${states.size}]")
      //states.toList.sortBy(_.remainingTime).foreach(println)
      println()
    }
    
    println(states.maxBy(_.flowSoFar))
    val res = states
      .filter(x => x.remainingTime1 == 0 && x.remainingTime2 == 0)
      .map(_.flowSoFar)
      .max
    println(res)
  }

  case class State(node: Valve, remainingTime: Int, flowSoFar: Int, opened: Set[Valve])

  case class State2(node1: Valve, node2: Valve, remainingTime1: Int, remainingTime2: Int, flowSoFar: Int, opened: Seq[Valve])


  case class Valve(id: String, rate: Int, leadsTo: Seq[String]) {
    override def toString = id
  }

  object Valve {
    type ID = String
    private val pattern = """Valve (.+) has flow rate=(.+); tunnel.? lead.? to valve.? (.+)""".r

    def of(s: String): Valve = s match {
      case pattern(id, rate, leadsTo) => Valve(id, rate.toInt, leadsTo.split(", "))
    }
  }

}




