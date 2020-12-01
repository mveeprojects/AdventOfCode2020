package day1

object AccountChecker extends App {

  def checkSumOfTwoNumbers(a: Int, b: Int): Boolean = a + b == 2020

  def checkListOfNumbers(inputList: List[Int]): Int = {
    val headValue = inputList.head
    inputList
      .tail
      .filter(checkSumOfTwoNumbers(headValue, _))
      .map(headValue * _)
      .head
  }
}
