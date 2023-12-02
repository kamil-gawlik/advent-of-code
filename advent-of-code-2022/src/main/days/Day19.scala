package days

import utils._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Day19 {
  def main(args: Array[String]): Unit = {
    part2()
  }

  /*
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
   */
  private def part2(): Unit = {
    val blueprints = readBlueprints.take(3)


    def maxForBlueprint(blueprint: Blueprint) = {
      val visited = mutable.Set.empty[State]
      var i = 0
      var states = Set(State(1, 0, 0, 0, 0, 0, 0, 0))
      while (i < 32) {
        println(s"minute: ${i + 1} [states:${states.size}]")
        val newStates = states.flatMap(s => nextStates(s, blueprint))
          .toList.sortBy(_.geodeRobots).reverse.take(3000000).toSet
        //.diff(visited) // no need to check them again, if already occurred
        states = newStates
        visited addAll newStates
        i = i + 1
      }
      states.maxBy(_.geodes).geodes
    }

    val levels = blueprints.map { b =>
      val res = maxForBlueprint(b)
      println(s"b: ${b.id} -> $res")
      res
    }
    println(levels.product)
  }
  
  private def part1(): Unit = {
    val blueprints = readBlueprints


    def maxForBlueprint(blueprint: Blueprint) = {
      val visited = mutable.Set.empty[State]
      var i = 0
      var states = Set(State(1, 0, 0, 0, 0, 0, 0, 0))
      while (i < 24) {
        //println(s"minute: ${i + 1} [states:${states.size}]")
        val newStates = states.flatMap(s => nextStates(s, blueprint))
          .toList.sortBy(_.geodeRobots).reverse.take(1000000).toSet
        //.diff(visited) // no need to check them again, if already occurred
        states = newStates
        visited addAll newStates
        i = i + 1
      }
      states.maxBy(_.geodes).geodes * blueprint.id
    }

    val levels = blueprints.map { b =>
      val res = maxForBlueprint(b)
      println(s"b: ${b.id} -> $res")
      res
    }
    println(levels.sum)
  }

  private def readBlueprints = readLines("day19.txt").map(Blueprint.of)

  def nextStates(state: State, blueprint: Blueprint): Set[State] = {
    val res = ArrayBuffer.empty[State]
    if (state.canBuildGeodeRobot(blueprint)) {
      res addOne state.buildGeodeRobot(blueprint)
    } else {
      if (state.canBuildOreRobot(blueprint)) {
        res addOne state.buildOreRobot(blueprint)
      }
      if (state.canBuildClayRobot(blueprint)) {
        res addOne state.buildClayRobot(blueprint)
      }
      if (state.canBuildObsidianRobot(blueprint)) {
        res addOne state.buildObsidianRobot(blueprint)
      }
      (res addOne state.next)
    }
    res.toSet
  }

  case class Blueprint(id: Int, oreRobotCostInOre: Int, clayRobotCostInOre: Int, obsidianRobotCostInOre: Int, obsidianRobotCostInClay: Int, geodeRobotCostInOre: Int, geodeRobotCostInObsidian: Int)

  case class State(oreRobots: Int, clayRobots: Int, obsidianRobots: Int, geodeRobots: Int, ores: Int, clays: Int, obsidians: Int, geodes: Int) {

    override def toString: String = s"State(orR:$oreRobots, clR:$clayRobots, obR:$obsidianRobots, geR:$geodeRobots | or:$ores, cl:$clays, ob:$obsidians, ge:$geodes )"

    def next(): State = {
      val s = this
      this.copy(ores = s.ores + s.oreRobots, clays = s.clays + s.clayRobots, obsidians = s.obsidians + s.obsidianRobots, geodes = s.geodes + s.geodeRobots)
    }

    def canBuildOreRobot(blueprint: Blueprint): Boolean = this.ores >= blueprint.oreRobotCostInOre

    def buildOreRobot(blueprint: Blueprint): State = {
      val s = this.next()
      s.copy(oreRobots = s.oreRobots + 1, ores = s.ores - blueprint.oreRobotCostInOre)
    }

    def canBuildClayRobot(blueprint: Blueprint): Boolean = this.ores >= blueprint.clayRobotCostInOre

    def buildClayRobot(blueprint: Blueprint): State = {
      val s = this.next()
      s.copy(clayRobots = s.clayRobots + 1, ores = s.ores - blueprint.clayRobotCostInOre)
    }

    def canBuildObsidianRobot(blueprint: Blueprint): Boolean = this.ores >= blueprint.obsidianRobotCostInOre && this.clays >= blueprint.obsidianRobotCostInClay

    def buildObsidianRobot(blueprint: Blueprint): State = {
      val s = this.next()
      s.copy(obsidianRobots = s.obsidianRobots + 1, ores = s.ores - blueprint.obsidianRobotCostInOre, clays = s.clays - blueprint.obsidianRobotCostInClay)
    }

    def canBuildGeodeRobot(blueprint: Blueprint): Boolean = this.ores >= blueprint.geodeRobotCostInOre && this.obsidians >= blueprint.geodeRobotCostInObsidian

    def buildGeodeRobot(blueprint: Blueprint): State = {
      val s = this.next()
      s.copy(geodeRobots = s.geodeRobots + 1, ores = s.ores - blueprint.geodeRobotCostInOre, obsidians = s.obsidians - blueprint.geodeRobotCostInObsidian)
    }
  }

  object Blueprint {
    private val pattern = """Blueprint (.+): Each ore robot costs (.+) ore. Each clay robot costs (.+) ore. Each obsidian robot costs (.+) ore and (.+) clay. Each geode robot costs (.+) ore and (.+) obsidian.""".r

    def of(s: String): Blueprint = s match {
      case pattern(id, oreRobotCostInOre, clayRobotCostInOre, obsidianRobotCostInOre, obsidianRobotCostInClay, geodeRobotCostInOre, geodeRobotCostInObidian) =>
        Blueprint(id.toInt, oreRobotCostInOre.toInt, clayRobotCostInOre.toInt, obsidianRobotCostInOre.toInt, obsidianRobotCostInClay.toInt, geodeRobotCostInOre.toInt, geodeRobotCostInObidian.toInt)
    }

  }
}
