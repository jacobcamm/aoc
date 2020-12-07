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
            .map(x => (x.split(" bag")(0).drop(2).trim, x.take(1).toInt))

        Bag(colour, Some(contains.toMap))
      }
    }
  }

  val input = readResource("Day7.txt")

  @tailrec
  def searchTopBags(colours: List[String],
                    currentPossibleBags: Set[Bag],
                    allBags: List[Bag]): Set[Bag] = {
    val bagsContainsColour =
      allBags
        .filter(
          x =>
            x.contains.fold(false)(y => y.exists(z => colours.contains(z._1)))
        )
        .toSet

    val allPossibilities = bagsContainsColour ++ currentPossibleBags

    val newPossibilities = allPossibilities -- currentPossibleBags

    if (newPossibilities.isEmpty) currentPossibleBags
    else
      searchTopBags(
        newPossibilities.map(_.colour).toList,
        allPossibilities,
        allBags
      )
  }

  def howManyBags(colour: String, allBags: List[Bag]): Int = {
    val currentBag = allBags.filter(_.colour == colour).head

    currentBag.contains match {
      case Some(x) =>
        x.map { k =>
          val bagsInBags = howManyBags(k._1, allBags)
          val totalBags = k._2 + (k._2 * bagsInBags)
          totalBags
        }.sum
      case None => 0
    }
  }

  val bags = input.map(Bag(_)).toList

  println(
    s"Day 7 Part 1: ${searchTopBags(List("shiny gold"), Set(), bags).size}"
  )

  println(s"Day 2 Part 2: ${howManyBags("shiny gold", bags)}")

}
