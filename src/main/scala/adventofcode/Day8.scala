package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): List[Instruction] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq
      .map(Instruction(_))
      .toList

  case class Instruction(command: String, distance: Int)

  object Instruction {
    def apply(input: String): Instruction = {
      val command = input.take(3)
      val signedValue = input.split(" ")(1)
      val distance = signedValue.take(1) match {
        case "+" => signedValue.drop(1).toInt
        case "-" => signedValue.drop(1).toInt * -1
      }

      Instruction(command, distance)
    }
  }

  @tailrec
  def getLatestNopOrJmp(oldPositions: List[Int],
                        instructions: List[Instruction],
                        changed: Int): Int = {
    instructions(oldPositions(oldPositions.size - 1 - changed)) match {
      case Instruction("nop", _) | Instruction("jmp", _) =>
        oldPositions(oldPositions.size - 1 - changed)
      case _ =>
        getLatestNopOrJmp(oldPositions.dropRight(1), instructions, changed)
    }
  }

  def updateInstruction(oldPositions: List[Int],
                        instructions: List[Instruction],
                        changed: Int): List[Instruction] = {
    val latestNopJmp = getLatestNopOrJmp(oldPositions, instructions, changed)

    val newInstruction = instructions(latestNopJmp) match {
      case Instruction("nop", distance) => Instruction("jmp", distance)
      case Instruction("jmp", distance) => Instruction("nop", distance)
    }
    instructions.updated(latestNopJmp, newInstruction)
  }

  @tailrec
  def searchInstructions(oldPositions: List[Int],
                         currentPosition: Int,
                         instructions: List[Instruction],
                         accumulator: Int,
                         changed: Int,
                         originalRun: List[Int]): Int = {
    if (oldPositions.contains(currentPosition)) {
      val orRun = if (originalRun == List()) oldPositions else originalRun
      searchInstructions(
        List(),
        0,
        updateInstruction(orRun, instructions, changed),
        0,
        changed + 1,
        orRun
      )
    } else if (currentPosition > instructions.size - 1) accumulator
    else {
      val newPositions = oldPositions :+ currentPosition
      val currentInstruction = instructions(currentPosition)
      currentInstruction match {
        case Instruction("acc", distance) =>
          searchInstructions(
            newPositions,
            currentPosition + 1,
            instructions,
            accumulator + distance,
            changed,
            originalRun
          )
        case Instruction("nop", _) =>
          searchInstructions(
            newPositions,
            currentPosition + 1,
            instructions,
            accumulator,
            changed,
            originalRun
          )
        case Instruction("jmp", distance) =>
          searchInstructions(
            newPositions,
            currentPosition + distance,
            instructions,
            accumulator,
            changed,
            originalRun
          )
      }
    }
  }

  val input = readResource("Day8.txt")

  val firstDouble = searchInstructions(List(), 0, input, 0, 0, List())
  println(input)

  println(s"Day 7 Part 1: $firstDouble")

  println(s"Day 2 Part 2:")

}
