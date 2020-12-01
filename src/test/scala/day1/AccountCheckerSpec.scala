package day1

import base.UnitSpecBase

class AccountCheckerSpec extends UnitSpecBase {

  val testAccChecker = new AccountChecker

  "checkSumOfTwoNumbers" should "return true if 2 numbers add up to 2020, otherwise return false" in {
    testAccChecker.checkSumOfTwoNumbers(2019, 1) shouldBe true
    testAccChecker.checkSumOfTwoNumbers(2019, 0) shouldBe false
  }

  "checkListOfNumbers" should "return a Some containing the product of the two numbers that add up to 2020 (if they exist)" in {
    val inputList = List(2000, 10, 20, 40)
    testAccChecker.checkListOfNumbers(inputList) shouldBe Some(40000)
  }

  "checkListOfNumbers" should "return a None if there are no number combinations that add up to 2020" in {
    val inputList = List(2000, 10, 30, 40)
    testAccChecker.checkListOfNumbers(inputList) shouldBe None
  }

  "readReport" should "return a list of ints if file is formatted correctly" in {
    testAccChecker.readReport("/day1/testinputgood") shouldBe Right(List(2000, 10, 20, 40))
  }

  "readReport" should "return an error message if file is not correctly formatted" in {
    testAccChecker.readReport("/day1/testinputbad") shouldBe Left("Something went wrong, make sure the input report exists at the path specified (/day1/testinputbad) and all lines are valid integers")
  }
}
