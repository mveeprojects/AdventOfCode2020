package day1

import helpers.FileReader.readInputFile

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Main extends App {
  val accChecker = new AccountChecker
  accChecker.run()
}

class AccountChecker {

  val defaultInputFilePath = "/day1/input"
  val numberToReach = 2020
  val qtyNumbersToSum = 3
  val intLineToString: String => Int = _.toInt

  def run(inputReportPath: String = defaultInputFilePath): Unit = {
    readInputFile(inputReportPath, intLineToString) match {
      case Right(report) =>
        if (qtyNumbersToSum == 2) checkListOfNumbersTwoNums(report) match {
          case Some(result) => println(s"Two numbers in the report did add up to $numberToReach, their product is $result")
          case None => println(s"No two numbers in the report added up to $numberToReach")
        } else checkListOfNumbersThreeNums(report) match {
          case Some(result) => println(s"Three numbers in the report did add up to $numberToReach, their product is $result")
          case None => println(s"No three numbers in the report added up to $numberToReach")
        }
      case Left(errorMessage) => println(errorMessage)
    }
  }

  def checkSumOfTwoNumbers(a: Int, b: Int): Boolean = a + b == numberToReach

  @tailrec
  final def checkListOfNumbersTwoNums(inputList: List[Int]): Option[Int] = {
    if (inputList.tail.size < 1) None
    else {
      val headValue = inputList.head
      Try(inputList.tail
        .filter(checkSumOfTwoNumbers(headValue, _))
        .map(headValue * _)
        .head) match {
        case Success(value) => Some(value)
        case Failure(_) => checkListOfNumbersTwoNums(inputList.tail)
      }
    }
  }

  def checkListOfNumbersThreeNums(inputList: List[Int]): Option[Int] = {
    Try(inputList
      .toSet[Int]
      .subsets(3)
      .toList
      .filter(_.sum == numberToReach)
      .head
      .product) match {
      case Success(value) => Some(value)
      case Failure(_) => None
    }
  }
}
