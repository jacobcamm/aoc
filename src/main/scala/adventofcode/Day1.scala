package adventofcode

import scala.io.Source

object Day1 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): Seq[Int] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq
      .map(_.toInt)

  val expenses = readResource("Day1.txt")

  def findProductOfSum(parts: Int,
                       target: Int,
                       data: List[Int]): Option[Int] = {
    if (parts == 1) return data.find(_ == target)
    for (position <- data.indices) {
      val currentValue = data(position)
      findProductOfSum(
        parts - 1,
        target - currentValue,
        data.drop(position + 1)
      ).map(x => return Some(x * currentValue))
    }
    None
  }

  def findProductOfSum2(parts: Int,
                        target: Int,
                        data: List[Int]): Option[Int] = {
    if (target < 0) None
    if (parts == 1) return data.find(_ == target)

    val currentValue = data.head
    val answer =
      findProductOfSum2(parts - 1, target - currentValue, data.tail)

    answer.fold(findProductOfSum2(parts, target, data.tail))(
      x => return Some(x * currentValue)
    )
  }

  // part 1
  val answer1 = for {
    first <- expenses
    minusFirst = expenses.tail
    second <- minusFirst
    total = first + second
    if total == 2020
  } yield first * second

  println(s"Day 1 Part 1 ${answer1.head}")
  println(findProductOfSum(2, 2020, expenses.toList))
  println(findProductOfSum2(2, 2020, expenses.toList))

  // part 2
  val answer2 = for {
    first <- expenses
    minusFirst = expenses.tail
    second <- minusFirst
    minusSecond = minusFirst.tail
    third <- minusSecond
    total = first + second + third
    if total == 2020
  } yield first * second * third

  println(s"Day 1 Part 2 ${answer2.head}")
  println(findProductOfSum(3, 2020, expenses.toList))
  println(findProductOfSum2(3, 2020, expenses.toList))

}
