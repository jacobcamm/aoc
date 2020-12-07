package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): Seq[String] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq

  case class Answer(yes: List[Char], people: Int)

  @tailrec
  def parseInput(input: List[String],
                 currentLine: String,
                 builtLine: String,
                 currentPeople: Int,
                 currentAnswers: List[Answer]): List[Answer] = {
    val newLine = builtLine + currentLine

    (currentLine, input.isEmpty) match {
      case (_, true) =>
        currentAnswers :+ Answer(newLine.toCharArray.toList, currentPeople + 1)
      case ("", _) =>
        parseInput(
          input.tail,
          input.head,
          "",
          0,
          currentAnswers :+ Answer(newLine.toCharArray.toList, currentPeople)
        )
      case _ =>
        parseInput(
          input.tail,
          input.head,
          newLine,
          currentPeople + 1,
          currentAnswers
        )
    }
  }

  val input = readResource("Day6.txt")

  val parsed = parseInput(input.tail.toList, input.head, "", 0, List())

  val groupedBy =
    parsed
      .map(x => x.yes.groupBy(identity).filter(_._2.size == x.people))

  println(s"Day 2 Part 1: ${parsed.map(_.yes.toSet.size).sum}")

  println(s"Day 2 Part 2: ${groupedBy.map(_.size).sum}")

}
