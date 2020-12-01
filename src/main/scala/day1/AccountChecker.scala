package day1

import java.io.File

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

object Main extends App {
  val accChecker = new AccountChecker
  accChecker.run()
}

class AccountChecker {

  val defaultInputFilePath = "/day1/input"
  val numberToReach = 2020

  def run(inputReportPath: String = defaultInputFilePath): Unit = {
    readReport(inputReportPath) match {
      case Right(report) => checkListOfNumbers(report) match {
        case Some(result) => println(s"Two numbers in the report did add up to $numberToReach, their product is $result")
        case None => println(s"No two numbers in the report added up to $numberToReach")
      }
      case Left(error) => println(error)
    }
  }

  def readReport(inputReportPath: String): Either[String, List[Int]] = {
    val file = new File(getClass.getResource(inputReportPath).getPath)
    val source: BufferedSource = Source.fromFile(file)
    Try(source.getLines.map(_.toInt).toList) match {
      case Success(value) =>
        source.close
        Right(value)
      case Failure(ex) =>
        source.close
        Left(s"Something went wrong, make sure the input report exists at the path specified ($inputReportPath) and all lines are valid integers")
    }
  }

  def checkSumOfTwoNumbers(a: Int, b: Int): Boolean = a + b == numberToReach

  @tailrec
  final def checkListOfNumbers(inputList: List[Int]): Option[Int] = {
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
