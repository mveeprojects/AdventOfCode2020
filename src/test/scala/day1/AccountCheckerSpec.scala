package day1

import base.UnitSpecBase
import day1.AccountChecker._

class AccountCheckerSpec extends UnitSpecBase {

  "checkSumOfTwoNumbers" should "return true if 2 numbers add up to 2020, otherwise return false" in {
    checkSumOfTwoNumbers(2019, 1) shouldBe true
    checkSumOfTwoNumbers(2019, 0) shouldBe false
  }

  "checkListOfNumbers" should "return a list of number combinations that add up to 2020" in {
    val inputList = List(2000, 10, 20, 40)
    checkListOfNumbers(inputList) shouldBe Some(40000)
  }

  "readReport" should "return a list of ints if file is formatted correctly" in {
    readReport("/day1/testinputgood") shouldBe Right(List(2000, 10, 20, 40))
  }

  "readReport" should "return an error message if file is not correctly formatted" in {
    readReport("/day1/testinputbad") shouldBe Left("Something went wrong, make sure all lines in input file are ints")
  }
}
