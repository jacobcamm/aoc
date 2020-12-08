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
                         originalRun: Option[List[Int]],
                         fixInstructions: Boolean): Int = {
    if (currentPosition > instructions.size - 1) accumulator
    else if (oldPositions.contains(currentPosition)) {
      if (fixInstructions) {
        val orRun = originalRun.getOrElse(oldPositions)
        searchInstructions(
          List(),
          0,
          updateInstruction(orRun, instructions, changed),
          0,
          changed + 1,
          Some(orRun),
          fixInstructions
        )
      } else accumulator
    } else {
      val newPositions = oldPositions :+ currentPosition
      val currentInstruction = instructions(currentPosition)
      val (newPosition, newAccumulator) = currentInstruction match {
        case Instruction("acc", distance) =>
          (currentPosition + 1, accumulator + distance)
        case Instruction("nop", _) =>
          (currentPosition + 1, accumulator)
        case Instruction("jmp", distance) =>
          (currentPosition + distance, accumulator)
      }
      searchInstructions(
        newPositions,
        newPosition,
        instructions,
        newAccumulator,
        changed,
        originalRun,
        fixInstructions
      )
    }
  }

  val input = readResource("Day8.txt")

  val firstDouble = searchInstructions(List(), 0, input, 0, 0, None, false)
  val fixedSolution = searchInstructions(List(), 0, input, 0, 0, None, true)

  println(s"Day 7 Part 1: $firstDouble")

  println(s"Day 2 Part 2: $fixedSolution")

}
