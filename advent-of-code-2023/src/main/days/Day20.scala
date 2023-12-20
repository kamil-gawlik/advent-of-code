package days

import days.Day20.Module
import utils.*

import java.time.OffsetTime
import scala.collection.immutable.Map

object Day20 {

  enum State {
    case on
    case off

    def flip(): State = this match {
      case State.on => off
      case State.off => on
    }
  }

  enum Pulse {
    case high
    case low
  }

  trait Module {
    val id: String

    def handle(p: Pulse, from: String): Option[Pulse]
  }

  case class Output(id: String = "output") extends Module {
    override def handle(p: Pulse, from: String): Option[Pulse] = None
  }

  case class Brodcast(override val id: String) extends Module {
    override def handle(p: Pulse, from: String): Option[Pulse] = Some(p)
  }

  case class FlipFlop(override val id: String, var state: State = State.off) extends Module {
    override def handle(p: Pulse, from: String): Option[Pulse] = {
      p match
        case Pulse.high => None
        case Pulse.low => {
          val res = this.state match {
            case State.on => Pulse.low
            case State.off => Pulse.high
          }
          this.state = this.state.flip()
          Some(res)
        }
    }
  }

  case class Conjunction(override val id: String) extends Module {

    var mem = Map.empty[String, Pulse]

    override def handle(p: Pulse, from: String): Option[Pulse] = {
      mem = mem + (from -> p)
      val allHigh = mem.values.forall(p => p == Pulse.high)
      if (allHigh) Some(Pulse.low)
      else Some(Pulse.high)
    }

    def setMem(connected: String) = {
      mem = mem + (connected -> Pulse.low)
    }
  }

  def readInput(): (Map[String, Module], Map[String, Seq[String]]) = {
    val lines = readLines("day20.txt")
    val pattern = """([%&])(.+) -> (.+)""".r
    val brodcastPattern = """broadcaster -> (.+)""".r

    def trimPeers(s: String): Seq[String] = s.split(",").map(_.trim)

    var conns = Map.empty[Module, Seq[String]]
    lines foreach {
      case brodcastPattern(peers) =>
        conns = conns + (Brodcast("broadcaster") -> trimPeers(peers))
      case pattern(modType, id, peers) => modType match {
        case "%" => conns = conns + (FlipFlop(id) -> trimPeers(peers))
        case "&" => conns = conns + (Conjunction(id) -> trimPeers(peers))
      }
    }
    val connMaps = conns.map((k, v) => k.id -> v)
    val modules = conns.keySet.map(m => m.id -> m).toMap
    // setup Conjuctions
    val conjunctions = conns.keySet.filter(m => m.isInstanceOf[Conjunction])
    conjunctions.foreach { c =>
      connMaps.foreach { (k, v) => {
        if (v.contains(c.id)) {
          modules(c.id).asInstanceOf[Conjunction].setMem(k)
        }
      }
      }
    }
    (modules, connMaps)
  }

  case class Signal(from: String, to: String, pulse: Pulse)

  case class Machine(modules: Map[String, Module], conns: Map[String, Seq[String]]) {

    def process(inspectFunction: Seq[Signal] => Unit = _ => (), debug: Boolean = false): (Long, Long) = {
      var lowC = 0L
      var highC = 0L

      def handleOne(s: Signal): Seq[Signal] = {
        if (s.pulse == Pulse.low) {
          lowC = lowC + 1
        } else highC = highC + 1
        if (debug) println(s"${s.from} -${s.pulse}-> ${s.to}")
        val processingModule = modules.getOrElse(s.to, Output())
        val pulse = processingModule.handle(s.pulse, s.from)
        pulse.map(p => conns(processingModule.id).map(to => Signal(processingModule.id, to, p))).getOrElse(Seq.empty[Signal])
      }

      var signals = Seq(Signal("button", "broadcaster", Pulse.low))
      while (signals.nonEmpty) {
        signals = signals.flatMap(handleOne)
        inspectFunction(signals)
      }
      (lowC, highC)
    }
  }


  def part1(): Long = {
    val count = 1000
    val (modules, conns) = readInput()
    val m = Machine(modules, conns)
    val countCycles = (0 until count).map(_ => m.process())

    def sumParallel(s: Seq[(Long, Long)]) = s.foldLeft((0L, 0L))((acc, curr) => (acc._1 + curr._1, acc._2 + curr._2))

    val (lowC, highC) = sumParallel(countCycles)
    lowC * highC
  }

  def part2(): Long = {
    val (modules, conns) = readInput()
    val m = Machine(modules, conns)
    val rxImmediateInput = conns.filter((k, v) => v.contains("rx")).keySet.head // there is only one
    val rxInputs = conns.filter((k, v) => v.contains(rxImmediateInput)).keySet

    def findCyclesNeededToFire(seeked: Set[String]): Map[String, Long] = {
      var i = 0L
      var found = Map.empty[String, Long]

      def inspectFunction(signals: Seq[Signal]): Unit = {
        signals.foreach { s =>
          if (seeked.contains(s.from) && s.pulse == Pulse.high) {
            found = found + (s.from -> i)
          }
        }
      }

      while (found.keys.toSet != seeked && i < 100_000) {
        i = i + 1
        m.process(inspectFunction)
      }
      found
    }

    val l = findCyclesNeededToFire(rxInputs)
    lcm(l.values.toSeq)
  }


  def main(args: Array[String]): Unit = {
    check(part1, 763500168L)
    check(part2, 207652583562007L)
  }
}
