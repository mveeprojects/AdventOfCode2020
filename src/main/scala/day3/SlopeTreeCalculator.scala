package day3

import helpers.FileReader.readInputFile

import scala.collection.mutable.ListBuffer

case class TobogganPosition(column: Int, row: Int)

object Main extends App {
  val slopeTreeCalculator = new SlopeTreeCalculator
  slopeTreeCalculator.run()
}

class SlopeTreeCalculator {

  val defaultInputFilePath: String = "/day3/input"
  val stringToChar: String => Array[Char] = s => s.toCharArray

  val matrixWidth: Int = 31
  val matrixHeight: Int = 323
  val movesRight: Int = 3
  val movesDown: Int = 1

  val listOfPositions: ListBuffer[TobogganPosition] = ListBuffer[TobogganPosition]()
  val tobogganPosition: TobogganPosition = TobogganPosition(0, 0)

  def run(): Unit = {

    listOfPositions += tobogganPosition
    for (i <- 0 until matrixHeight - 1) {
      listOfPositions += TobogganPosition(calculateNextColumnPosition(tobogganPosition), i)
    }
    val positions = listOfPositions.toList

    generateSlopeMatrix() match {
      case Some(matrix) =>
        val treeCount = positions.map { pos =>
          matrix(pos.row)(pos.column)
        }.count(checkIfTree)
        println(s"Number of trees hit: $treeCount")
      case None => println("Something went horribly wrong")
    }
  }

  def generateSlopeMatrix(): Option[Array[Array[Char]]] = {
    readInputFile(defaultInputFilePath, stringToChar) match {
      case Right(matrixRows: List[Array[Char]]) => Some(matrixRows.toArray)
      case Left(errorMessage) =>
        println(errorMessage)
        None
    }
  }

  def calculateNextColumnPosition(currentPosition: TobogganPosition): Int = {
    val currentColumn = currentPosition.column
    if (currentColumn >= (matrixWidth - movesRight)) {
      val remainingMovesRightAfterEdge: Int = (currentColumn + movesRight) - (matrixWidth - 1)
      remainingMovesRightAfterEdge
    } else currentColumn + movesRight
  }

  def checkIfTree(c: Char): Boolean = {
    if (c == '#') true
    else false
  }
}
