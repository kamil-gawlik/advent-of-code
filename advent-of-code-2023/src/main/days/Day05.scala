package days

import com.google.common.collect.{RangeMap, TreeRangeMap, Range as JRange}
import utils.*
import scala.jdk.CollectionConverters._

import java.math.BigInteger

// requires com.google.guava
object Day05 {

  type I = BigInt
  type RM = RangeMap[I, JRange[I]]

  case class Almanac(seedToSoil: RM,
                     soilToFertilizer: RM,
                     fertilizerToWater: RM,
                     waterToLight: RM,
                     lightToTemp: RM,
                     tempToHumid: RM,
                     humidityToLocation: RM) {
    def getLocation(seed: I): I =  {
      humidityToLocation.getCorrespondingValue(
        tempToHumid.getCorrespondingValue(
          lightToTemp.getCorrespondingValue(
            waterToLight.getCorrespondingValue(
              fertilizerToWater.getCorrespondingValue(
                soilToFertilizer.getCorrespondingValue(
                  seedToSoil.getCorrespondingValue(seed)
                ))))))
    }
  }

  extension (m: RM)
    def getCorrespondingValue(seeked: I): I = {
      val entry = m.getEntry(seeked)
      if (entry == null) {
        return seeked
      }
      val correspondingValue = (seeked - entry.getKey.lowerEndpoint()) + entry.getValue.lowerEndpoint
      correspondingValue
    }

  def buildRangeMap(lines: Array[String]): RM = {
    val l = lines.tail
    val m: RM = TreeRangeMap.create()
    l.foreach { ll =>
      val (destRangeStart, sourceRangeStart, span) = {
        val split = ll.split(" ")
        (BigInt(split(0)), BigInt(split(1)), BigInt(split(2)))
      }
      m.put(JRange.closed(sourceRangeStart, sourceRangeStart + span - 1), JRange.closed(destRangeStart, destRangeStart + span - 1))
    }
    m
  }

  def readData(): (Seq[I], Almanac) = {
    val lines = readLinesSplitEmptyLine("day05.txt")
    val seeds = lines(0)(0).replace("seeds:", "").split(" ").map(_.trim).filter(_.nonEmpty).map(BigInt.apply)
    val seedToSoil = buildRangeMap(lines(1))
    val soilToFertilizer = buildRangeMap(lines(2))
    val fertilizerToWater = buildRangeMap(lines(3))
    val waterToLight = buildRangeMap(lines(4))
    val lightToTemp = buildRangeMap(lines(5))
    val tempToHumid = buildRangeMap(lines(6))
    val hubitToLocation = buildRangeMap(lines(7))
    (seeds, Almanac(seedToSoil, soilToFertilizer, fertilizerToWater, waterToLight, lightToTemp, tempToHumid, hubitToLocation))
  }

  def part1(): I = {
    val (seeds, almanac) = readData()
    seeds.map { s =>
      almanac.getLocation(s)
    }.min
  }

  def part2(): I = {
    val (seeds, almanac) = readData()
    val ss = seeds.grouped(2).flatMap(sss => (sss.head to (sss.head + sss(1)) by 1))
    ss.map { s =>
      almanac.getLocation(s)
    }.min
  }

  def main(args: Array[String]): Unit = {
    println(part2())
  }
}
