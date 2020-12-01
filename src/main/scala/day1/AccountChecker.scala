package day1

import java.io.File

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

object AccountChecker extends App {

  val defaultInputFilePath = "/day1/input"
  val numberToReach = 2020

  readReport() match {
    case Right(report) => checkListOfNumbers(report) match {
      case Some(result) => println(s"Two numbers did add up to $numberToReach, their product is $result")
      case None => println(s"No numbers in the input list add up to $numberToReach")
    }
    case Left(error) => println(error)
  }

  def readReport(resourcePath: String = defaultInputFilePath): Either[String, List[Int]] = {
    val file = new File(getClass.getResource(resourcePath).getPath)
    val source: BufferedSource = Source.fromFile(file)
    Try(source.getLines.map(_.toInt).toList) match {
      case Success(value) =>
        source.close
        Right(value)
      case Failure(_) =>
        source.close
        Left("Something went wrong, make sure all lines in input file are ints")
    }
  }

  def checkSumOfTwoNumbers(a: Int, b: Int): Boolean = {
    a + b == numberToReach
  }

  @tailrec
  def checkListOfNumbers(inputList: List[Int]): Option[Int] = {
    if (inputList.tail.size < 1) None
    else {
      val headValue = inputList.head
      Try(inputList.tail
        .filter(checkSumOfTwoNumbers(headValue, _))
        .map(headValue * _)
        .head) match {
        case Success(value) => Some(value)
        case Failure(_) => checkListOfNumbers(inputList.tail)
      }
    }
  }
}
