package days


import days.Day19.Result
import days.Day19.Rule.Return
import utils.*

object Day19 {

  type Id = String

  case class Part(properties: Map[String, Long]) {
    def calculateValue: Long = properties.values.sum
  }

  object Part {
    private val pattern = """\{x=(.+),m=(.+),a=(.+),s=(.+)\}""".r

    def of(s: String): Part = s match {
      case pattern(x, m, a, s) => Part
        (Map("x" -> x.toLong,
          "m" -> m.toLong,
          "a" -> a.toLong,
          "s" -> s.toLong))
    }
  }

  case class Workflow(id: Id, rules: Seq[Rule]) {
    def eval(part: Part): Result =
      rules.foldLeft((part, Option.empty[Result])) {
        (partResult, rule) =>
          val (part, res) = partResult
          if (res.isDefined) {
            (part, res)
          } else (part, rule.eval(part))
      }._2.get

    /*def search(range: PartRange): Long = {
      val (r, v) = rules.foldLeft((range, 1L)) {
        (rangeAndVal, rule) =>
          val (range, value) = rangeAndVal
          val (accepted, forwarded) = rule.acceptedRange(range)
          (forwarded, value * accepted.product)
      }
      r.product * v
    }*/
  }

  object Workflow {

    private val pattern = """(.+)\{(.+)\}""".r

    def of(s: String): Workflow = s match {
      case pattern(id, rules) => Workflow(id, rules.split(",").map(Rule.of))
    }
  }

  enum Rule {
    case Lower(property: String, value: Long, result: Result)
    case Bigger(property: String, value: Long, result: Result)
    case Return(result: Result)

    def eval(p: Part): Option[Result] = this match {
      case l: Lower => if (p.properties(l.property) < l.value) {
        Some(l.result)
      } else None
      case b: Bigger => if (p.properties(b.property) > b.value) {
        Some(b.result)
      } else None
      case r: Return => Some(r.result)
    }

    // (accepted, forwarded)
    def acceptedRange(pr: PartRange): ((PartRange, Result), PartRange) = this match {
      case l: Lower => (pr.lower(l.property, l.value - 1) -> l.result, pr.higher(l.property, l.value))
      case b: Bigger => (pr.higher(b.property, b.value + 1) -> b.result, pr.lower(b.property, b.value))
      case r: Return => (pr -> r.result, pr)
    }
  }

  object Rule {

    private val ruleLowerPattern = """(.+)<(.+):(.+)""".r
    private val ruleBiggerPattern = """(.+)>(.+):(.+)""".r

    def of(s: String): Rule = s match
      case ruleLowerPattern(prop, value, res) => Lower(prop, value.toLong, Result.of(res))
      case ruleBiggerPattern(prop, value, res) => Bigger(prop, value.toLong, Result.of(res))
      case a => Return(Result.of(a))
  }

  enum Result {
    case Next(id: Id)
    case Accepted
    case Rejected
  }

  object Result {
    def of(s: String): Result = s match {
      case "A" => Accepted
      case "R" => Rejected
      case id => Next(id)
    }
  }

  case class LRange(start: Long, end: Long) {
    def len: Long = end - start + 1
  }

  case class PartRange(ranges: Map[String, LRange]) {

    def min(l1: Long, l2: Long): Long = if (l1 < l2) l1 else l2

    def max(l1: Long, l2: Long): Long = if (l1 < l2) l2 else l1

    def intersect(r1: LRange, r2: LRange): LRange = {
      val start = max(r1.start, r2.start)
      val end = max(min(r1.end, r2.end), start)
      LRange(start, end)
    }

    def lower(p: String, v: Long): PartRange = {
      //PartRange(ranges + (p -> LRange(PartRange.MIN, min(ranges(p).end, v))))
      PartRange(ranges + (p -> intersect(ranges(p), LRange(PartRange.MIN, v))))
    }

    def higher(p: String, v: Long): PartRange = {
      //PartRange(ranges + (p -> LRange(max(ranges(p).start, v), PartRange.MAX)))
      PartRange(ranges + (p -> intersect(ranges(p), LRange(v, PartRange.MAX))))
    }


    def combinations: Long = ranges("x").len * ranges("m").len * ranges("a").len * ranges("s").len
  }

  object PartRange {
    val MIN = 1L
    val MAX = 4000L
    val initial = PartRange(Map(
      "x" -> LRange(MIN, MAX),
      "m" -> LRange(MIN, MAX),
      "a" -> LRange(MIN, MAX),
      "s" -> LRange(MIN, MAX)
    ))

    val ONE = LRange(1L, 1L)

    def product(r1: LRange, r2: LRange): Long =
      (r1.end - r1.start) * (r2.end - r2.start)

  }

  def part1(): Long = {
    val lines = readLinesSplitEmptyLine("day19.txt")
    val workflowsMap = lines(0).map(Workflow.of).map(w => w.id -> w).toMap
    val parts = lines(1).map(Part.of)
    val p = parts(0)

    def workflowEvaluator(w: Workflow, p: Part): Long = {
      w.eval(p) match {
        case Result.Next(id) => workflowEvaluator(workflowsMap(id), p)
        case Result.Accepted => p.calculateValue
        case Result.Rejected => 0L
      }
    }

    parts.map(p => workflowEvaluator(workflowsMap("in"), p)).sum
  }

  def part2(): Long = {
    val lines = readLinesSplitEmptyLine("day19.txt")
    val workflowsMap = lines(0).map(Workflow.of).map(w => w.id -> w).toMap

    def workflowSearch(workflow: Workflow, partRange: PartRange): Long = {
      var sum = 0L
      var forwarded = partRange
      //println(s"[in ${workflow.id}] range: $partRange ")
      for (r <- workflow.rules) {
        val ((accepted, result), nextForwarded) = r.acceptedRange(forwarded)
        forwarded = nextForwarded
        //println(s"[out ${workflow.id}] rule: $r acc: ($accepted, res: $result), forw: $nextForwarded ")
        sum = sum + recurse(result, accepted)
      }
      //println(s"[SUM ${workflow.id}] range: $partRange sum: $sum")
      sum
    }

    def recurse(result: Result, forwarded: PartRange): Long = {
      val res = (result, forwarded) match {
        case (Result.Next(id), forwarded) => if (forwarded.combinations == 0L) {
          0L
        } else {
          workflowSearch(workflowsMap(id), forwarded)
        }
        case (Result.Accepted, forwarded) => forwarded.combinations
        case (Result.Rejected, _) => 0L
      }
      //println(s"recurse: $result $forwarded $res")
      res
    }

    workflowSearch(workflowsMap("in"), PartRange.initial)
  }

  def main(args: Array[String]): Unit = {
    check(part1, 432427)
    check(part2, 143760172569135L)
  }
}
