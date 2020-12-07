package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): Seq[String] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq

  case class Bag(colour: String, contains: Option[Map[String, Int]])

  object Bag {
    def apply(input: String): Bag = {
      val split = input.split("bags contain ")
      val colour = split(0).trim
      if (split(1).trim == "no other bags.") Bag(colour, None)
      else {
        val contains =
          split(1)
            .split(", ")
            .map(x => (x.split(" bags")(0).drop(2).trim, x.take(1).toInt))

        Bag(colour, Some(contains.toMap))
      }

    }
  }

  val input = readResource("Day7.txt")

  println(input.map(Bag(_)))

  println(s"Day 2 Part 1: ")

  println(s"Day 2 Part 2: ")

}
