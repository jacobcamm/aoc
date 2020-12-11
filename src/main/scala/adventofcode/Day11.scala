package adventofcode

import adventofcode.Utils._

import scala.annotation.tailrec
import scala.io.Source
import scala.math._

object Day11 extends App {

  /**
    * Read the contents of a resource file given its relative path into a sequence of lines
    */
  def readResource(resource: String): List[List[Char]] =
    Source
      .fromInputStream(getClass.getClassLoader.getResourceAsStream(resource))
      .getLines()
      .toSeq
      .toList
      .map(_.toCharArray.toList)

  def areasToCheck(current: Int, max: Int): List[Int] = {
    (current, current == max - 1) match {
      case (0, _)    => List(0, 1)
      case (_, true) => List(current - 1, current)
      case _         => List(current - 1, current, current + 1)
    }
  }

  def vertical(start: Int,
               end: Int,
               column: Int,
               down: Boolean): List[(Int, Int)] = {
    (for {
      current <- if (down) start + 1 to end else 0 until start
    } yield (current, column)).toList
  }

  def horizontal(start: Int,
                 end: Int,
                 row: Int,
                 right: Boolean): List[(Int, Int)] = {
    (for {
      current <- if (right) start + 1 to end else 0 until start
    } yield (row, current)).toList
  }

  def diagonalBottomRight(startRow: Int,
                          endRow: Int,
                          startColumn: Int,
                          endColumn: Int): List[(Int, Int)] = {
    val diff = startColumn - startRow
    (for {
      currentColumn <- startColumn + 1 to endColumn
      currentRow <- startRow + 1 to endRow
      if currentColumn - currentRow == diff
    } yield (currentRow, currentColumn)).toList
  }

  def diagonalTopRight(startRow: Int,
                       startColumn: Int,
                       endColumn: Int): List[(Int, Int)] = {
    val total = startColumn + startRow
    (for {
      currentColumn <- startColumn + 1 to endColumn
      currentRow <- (0 to startRow).reverse
      if currentColumn + currentRow == total
    } yield (currentRow, currentColumn)).toList
  }

  def diagonalTopLeft(startRow: Int, startColumn: Int): List[(Int, Int)] = {
    val diff = startColumn - startRow
    (for {
      currentColumn <- (0 until startColumn).reverse
      currentRow <- (0 until startRow).reverse
      if currentColumn - currentRow == diff
    } yield (currentRow, currentColumn)).toList
  }

  def diagonalBottomLeft(startRow: Int,
                         endRow: Int,
                         startColumn: Int): List[(Int, Int)] = {
    val total = startColumn + startRow
    (for {
      currentColumn <- (0 to startColumn).reverse
      currentRow <- startRow + 1 to endRow
      if currentColumn + currentRow == total
    } yield (currentRow, currentColumn)).toList
  }

  def updateSeat(row: Int,
                 column: Int,
                 newValue: Char,
                 seats: List[List[Char]]): List[List[Char]] = {
    val updatedColumn = seats(row).updated(column, newValue)
    seats.updated(row, updatedColumn)
  }

  def sightlineCheck(currentRow: Int,
                     currentColumn: Int,
                     currentPositions: List[List[Char]],
                     sightLines: Map[(Int, Int), List[(Int, Int)]]): Int = {
    val spacesToCheck =
      sightLines.get(currentRow, currentColumn).get

    spacesToCheck
      .map(x => currentPositions(x._1)(x._2) == '#')
      .count(_ == true)
  }

  @tailrec
  def changeSeats(currentPositions: List[List[Char]],
                  newPositions: List[List[Char]],
                  currentRow: Int,
                  currentColumn: Int,
                  changed: Boolean,
                  checkSeats: (Int, Int, List[List[Char]]) => Int,
                  seatLimit: Int): List[List[Char]] = {
    if (currentRow >= currentPositions.size && !changed) newPositions
    else if (currentRow >= currentPositions.size) {
      changeSeats(
        newPositions,
        newPositions,
        0,
        0,
        false,
        checkSeats,
        seatLimit
      )
    } else {
      val currentSeat = currentPositions(currentRow)(currentColumn)
      val (nextRow, nextColumn) =
        if (currentColumn == currentPositions(currentRow).size - 1) {
          (currentRow + 1, 0)
        } else {
          (currentRow, currentColumn + 1)
        }

      if (currentSeat == '.')
        changeSeats(
          currentPositions,
          newPositions,
          nextRow,
          nextColumn,
          changed,
          checkSeats,
          seatLimit
        )
      else {
        val countOccupied =
          checkSeats(currentRow, currentColumn, currentPositions)

        val newSeat = (countOccupied, currentSeat) match {
          case (x, 'L') if x == 0         => '#'
          case (x, '#') if x >= seatLimit => 'L'
          case _                          => currentSeat
        }

        if (newSeat == currentSeat)
          changeSeats(
            currentPositions,
            newPositions,
            nextRow,
            nextColumn,
            changed,
            checkSeats,
            seatLimit
          )
        else {
          changeSeats(
            currentPositions,
            updateSeat(currentRow, currentColumn, newSeat, newPositions),
            nextRow,
            nextColumn,
            true,
            checkSeats,
            seatLimit
          )
        }
      }
    }
  }

  def countOccupied(seats: List[List[Char]]): Int = {
    (for {
      row <- seats
      seat <- row
      if seat == '#'
    } yield 1).sum
  }

  def createSightLines(row: Int,
                       column: Int,
                       maxRow: Int,
                       maxColumn: Int,
                       seats: List[List[Char]]): List[(Int, Int)] = {
    def find(input: List[(Int, Int)]): Option[(Int, Int)] = {
      input.find(x => seats(x._1)(x._2) == '#' || seats(x._1)(x._2) == 'L')
    }

    val down = find(vertical(row, maxRow, column, true))
    val up = find(vertical(row, maxRow, column, false).reverse)

    val right = find(horizontal(column, maxColumn, row, true))
    val left = find(horizontal(column, maxColumn, row, false).reverse)

    val diagonalUpRight = find(diagonalTopRight(row, column, maxColumn))
    val diagonalUpLeft = find(diagonalTopLeft(row, column))
    val diagonalDownRight = find(
      diagonalBottomRight(row, maxRow, column, maxColumn)
    )
    val diagonalDownLeft = find(diagonalBottomLeft(row, maxRow, column))
    List(
      down,
      up,
      right,
      left,
      diagonalUpRight,
      diagonalUpLeft,
      diagonalDownRight,
      diagonalDownLeft
    ).flatten
  }

  val input = readResource("Day11.txt")

  val positionToSightLines = time(
    (for {
      row <- input.indices
      column <- input.head.indices
      if input(row)(column) != '.'
    } yield
      (row, column) -> createSightLines(
        row,
        column,
        input.size - 1,
        input.head.size - 1,
        input
      )).toMap
  )

  def createAdjacent(row: Int,
                     column: Int,
                     seats: List[List[Char]]): List[(Int, Int)] = {
    val rowsToCheck = areasToCheck(row, seats.size)
    val columnsToCheck = areasToCheck(column, seats.head.size)

    val checks = for {
      rowCheck <- rowsToCheck
      columnCheck <- columnsToCheck
      if rowCheck != row || columnCheck != column
    } yield (rowCheck, columnCheck)

    checks.filter(x => seats(x._1)(x._2) == '#' || seats(x._1)(x._2) == 'L')
  }

  val adjacentPositions = (for {
    row <- input.indices
    column <- input.head.indices
    if input(row)(column) != '.'
  } yield (row, column) -> createAdjacent(row, column, input)).toMap

  def sightlineCheckWithSights = sightlineCheck(_, _, _, positionToSightLines)
  def nextToCheckWithLimits = sightlineCheck(_, _, _, adjacentPositions)

  val changedSeatsNormal =
    time(changeSeats(input, input, 0, 0, false, nextToCheckWithLimits, 4))

  val changedSeatsComp =
    time(changeSeats(input, input, 0, 0, false, sightlineCheckWithSights, 5))

  println(s"Day 11 Part 1: ${countOccupied(changedSeatsNormal)}")

  println(s"Day 11 Part 2: ${countOccupied(changedSeatsComp)}")

}
