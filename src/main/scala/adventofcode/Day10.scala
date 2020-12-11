package adventofcode

import adventofcode.Utils._

import scala.annotation.tailrec
import scala.io.Source
import scala.math._

object Day10 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): List[Int] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq
      .toList
      .map(_.toInt)

  @tailrec
  def checkNextVoltage(currentVoltage: Int,
                       voltageList: List[Int],
                       oneJumps: Int,
                       threeJumps: Int): BigDecimal = {
    if (voltageList.isEmpty) oneJumps * (threeJumps + 1)
    else {
      voltageList.head - currentVoltage match {
        case 1 =>
          checkNextVoltage(
            voltageList.head,
            voltageList.tail,
            oneJumps + 1,
            threeJumps
          )
        case 2 =>
          checkNextVoltage(
            voltageList.head,
            voltageList.tail,
            oneJumps,
            threeJumps
          )
        case 3 =>
          checkNextVoltage(
            voltageList.head,
            voltageList.tail,
            oneJumps,
            threeJumps + 1
          )
      }
    }
  }

  @tailrec
  def checkPossibilities(
    currentVoltage: Int,
    voltageList: List[Int],
    previousPossibilities: Map[Int, BigDecimal]
  ): BigDecimal = {
    val voltValue = previousPossibilities.getOrElse(
      currentVoltage - 1,
      BigDecimal(0)
    ) + previousPossibilities.getOrElse(currentVoltage - 2, BigDecimal(0)) + previousPossibilities
      .getOrElse(currentVoltage - 3, BigDecimal(0))

    if (voltageList.isEmpty) voltValue
    else {
      //println(s"$currentVolt $voltValue $previousPossibilities")
      checkPossibilities(
        voltageList.head,
        voltageList.tail,
        previousPossibilities + (currentVoltage -> voltValue)
      )
    }
  }

  val input = readResource("Day10.txt")

  val sorted = input.sorted

  println(s"Day 7 Part 1: ${time(checkNextVoltage(0, sorted, 0, 0))}")

  println(
    s"Day 2 Part 2: ${time(checkPossibilities(sorted.head, sorted.tail, Map(0 -> BigDecimal(1))))}"
  )

  def checkDif(current: Int,
               remaining: List[Int],
               currentDiffs: List[Int]): List[Int] = {
    if (remaining.isEmpty) currentDiffs
    else
      checkDif(
        remaining.head,
        remaining.tail,
        currentDiffs :+ remaining.head - current
      )
  }

  println(checkDif(sorted.head, sorted.tail, List()))

}
