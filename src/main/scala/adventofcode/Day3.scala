package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): Seq[String] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq

  val input = readResource("Day3.txt")
  val withIndex = input.zipWithIndex.toList

  @tailrec
  def reduceSize(current: Int, max: Int): Int = {
    if (current < max) current
    else reduceSize(current - max, max)
  }

  def checkTrees(input: List[(String, Int)], right: Int, down: Int): Int = {

    val filteredInput = input
      .filter {
        case (_, 0) => true
        case (_, i) => i % down == 0
      }
      .map(_._1)
      .zipWithIndex

    val hits =
      for {
        (row, pos) <- filteredInput
        posToCheck = reduceSize(pos * right, row.length)
        if row.charAt(posToCheck) == '#'
      } yield true

    hits.count(_ == true)
  }

  val checkTreesWithInput = checkTrees(withIndex, _, _)

  val attempts = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

  println(s"Day 2 Part 1: ${checkTrees(withIndex, 3, 1)}")

  println(
    s"Day 2 Part 2: ${attempts.map(x => checkTreesWithInput(x._1, x._2)).product}"
  )

}
