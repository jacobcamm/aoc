package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day9 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): List[BigDecimal] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq
      .toList
      .map(BigDecimal(_))

  val input = readResource("Day9.txt")

  @tailrec
  def findWeakness(prev25: List[BigDecimal],
                   currentValue: BigDecimal,
                   remainingList: List[BigDecimal]): BigDecimal = {
    val possibleSum =
      prev25
        .take(25)
        .filter(_ != currentValue)
        .combinations(2)
        .toList
        .filter(x => x.head != x.last && x.head + x.last == currentValue)

    if (possibleSum.isEmpty) currentValue
    else if (remainingList.isEmpty) currentValue
    else
      findWeakness(
        prev25.drop(1) :+ currentValue,
        remainingList.head,
        remainingList.tail
      )

  }

  @tailrec
  def exploitWeakness(values: List[BigDecimal],
                      currentPos: Int,
                      target: BigDecimal): BigDecimal = {
    val currentValues = values.take(currentPos)
    val currentSum = currentValues.sum

    currentSum match {
      case currentSum if currentSum == target =>
        currentValues.max + currentValues.min
      case currentSum if currentSum > target =>
        exploitWeakness(values.drop(1), 0, target)
      case _ => exploitWeakness(values, currentPos + 1, target)
    }
  }

  val weakNumber: BigDecimal =
    time(findWeakness(input.take(25), input(25), input.drop(26)))
  val exploitWeakness: BigDecimal = time(exploitWeakness(input, 0, weakNumber))

  def time[R](block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block // call-by-name
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }
  println(s"Day 7 Part 1: $weakNumber")

  println(s"Day 2 Part 2: $exploitWeakness")

}
