package adventofcode

import scala.io.Source

object Day2 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): Seq[Rule] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq
      .map(Rule(_))

  // part 1
  case class Rule(min: Int, max: Int, letter: Char, password: String) {
    def checkPasswordCount: Boolean = {
      val count = password.filter(_ == letter).length
      min <= count && count <= max
    }

    // part 2
    def checkPasswordPos: Boolean = {
      val firstChar = password.charAt(min - 1)
      val lastChar = password.charAt(max - 1)

      firstChar == letter ^ lastChar == letter
    }
  }

  object Rule {
    def apply(input: String): Rule = {
      val splitStrings = input.split(" ")
      val (nums, let, password) =
        (splitStrings(0), splitStrings(1), splitStrings(2))
      val min = nums.split("-")(0).toInt
      val max = nums.split("-")(1).toInt
      val letter = let.charAt(0)
      Rule(min, max, letter, password)
    }
  }

  println(
    s"Day 2 Part 1: ${readResource("Day2.txt").map(_.checkPasswordCount).count(_ == true)}"
  )

  println(
    s"Day 2 Part 2: ${readResource("Day2.txt").map(_.checkPasswordPos).count(_ == true)}"
  )

}
