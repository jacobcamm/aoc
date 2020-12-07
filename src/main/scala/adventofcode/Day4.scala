package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): Seq[String] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq

  val input = readResource("Day4.txt")

  case class Passport(byr: Option[Int],
                      iyr: Option[Int],
                      eyr: Option[Int],
                      hgt: Option[String],
                      hcl: Option[String],
                      ecl: Option[String],
                      pid: Option[String],
                      cid: Option[String]) {

    val legalEyeColours = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

    def legal: Boolean = {
      val byrLegal = byr.fold(false)(x => x >= 1920 && x <= 2002)
      val iyrLegal = iyr.fold(false)(x => x >= 2010 && x <= 2020)
      val eyrLegal = eyr.fold(false)(x => x >= 2020 && x <= 2030)
      val hgtLegal = hgt.fold(false) { x =>
        val size = x.dropRight(2)
        val measure = x.takeRight(2)
        measure match {
          case "cm" => size.toInt >= 150 && size.toInt <= 193
          case "in" => size.toInt >= 59 && size.toInt <= 76
          case _    => false
        }
      }
      val hclLegal = hcl.fold(false) { x =>
        x.take(1) == "#" && x
          .drop(1)
          .forall(_.isLetterOrDigit) && x.drop(1).length == 6
      }
      val eclLegal = ecl.fold(false)(legalEyeColours.contains(_))
      val pidLegal = pid.fold(false) { x =>
        x.length == 9 && x.forall(_.isDigit)
      }

      byrLegal && iyrLegal && eyrLegal && hgtLegal && hclLegal && eclLegal && pidLegal
    }

    def looseLegal: Boolean = {
      byr.isDefined && iyr.isDefined && eyr.isDefined && hgt.isDefined && hcl.isDefined && ecl.isDefined && pid.isDefined
    }
  }

  object Passport {
    def apply(input: Map[String, String]): Passport = {
      Passport(
        input.get("byr").map(_.toInt),
        input.get("iyr").map(_.toInt),
        input.get("eyr").map(_.toInt),
        input.get("hgt"),
        input.get("hcl"),
        input.get("ecl"),
        input.get("pid"),
        input.get("cid")
      )
    }
  }

  def addToMap(currentMap: Map[String, String],
               input: String): Map[String, String] = {
    val split = input.split(":")
    currentMap + (split(0) -> split(1))
  }

  @tailrec
  def newMap(currentMap: Map[String, String],
             currentInput: String,
             remainingInput: List[String]): Map[String, String] = {
    if (remainingInput.isEmpty) addToMap(currentMap, currentInput)
    else
      newMap(
        addToMap(currentMap, currentInput),
        remainingInput.head,
        remainingInput.tail
      )
  }

  @tailrec
  def parseInput(input: List[String],
                 currentLine: String,
                 currentMap: Map[String, String],
                 currentPassports: List[Passport]): List[Passport] = {
    val splitLine = currentLine.split(" ")

    (currentLine, input) match {
      case (_, input) if input.isEmpty =>
        currentPassports :+ Passport(
          newMap(currentMap, splitLine.head, splitLine.tail.toList)
        )
      case ("", input) =>
        parseInput(
          input.tail,
          input.head,
          Map.empty,
          currentPassports :+ Passport(currentMap)
        )
      case _ =>
        parseInput(
          input.tail,
          input.head,
          newMap(currentMap, splitLine.head, splitLine.tail.toList),
          currentPassports
        )
    }
  }

  val parsed = parseInput(input.tail.toList, input.head, Map.empty, List())

  println(parsed)

  val looseLegalPassports = parsed.map(_.looseLegal).count(_ == true)
  val legalPassports = parsed.map(_.legal).count(_ == true)

  println(s"Day 2 Part 1: $looseLegalPassports")

  println(s"Day 2 Part 2: $legalPassports")

}
