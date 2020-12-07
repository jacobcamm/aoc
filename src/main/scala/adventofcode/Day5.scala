package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): Seq[List[Char]] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq
      .map(_.toCharArray.toList)

  @tailrec
  def findSeat(min: Int, max: Int, letter: Char, remaining: List[Char]): Int = {
    val difference = max - min + 1
    (letter, remaining.isEmpty) match {
      case ('F' | 'L', true) => min
      case ('F' | 'L', _) =>
        findSeat(min, max - difference / 2, remaining.head, remaining.tail)
      case ('B' | 'R', true) => max
      case ('B' | 'R', _) =>
        findSeat(min + difference / 2, max, remaining.head, remaining.tail)
    }
  }

  val input = readResource("Day5.txt")

  val ids = input.map { x =>
    val rows = x.take(7)
    val columns = x.takeRight(3)
    val row = findSeat(0, 127, rows.head, rows.tail)
    val column = findSeat(0, 7, columns.head, columns.tail)
    row * 8 + column
  }

  val sortedIds = ids.sorted

  @tailrec
  def findGap(currentNum: Int,
              previousNum: Int,
              remainingNums: List[Int]): Int = {
    if (previousNum + 1 == currentNum)
      findGap(remainingNums.head, currentNum, remainingNums.tail)
    else
      previousNum + 1
  }

  val missingNum =
    findGap(sortedIds.head, sortedIds.head - 1, sortedIds.tail.toList)

  println(s"Day 2 Part 1: ${ids.max}")

  println(s"Day 2 Part 2: $missingNum")

}
