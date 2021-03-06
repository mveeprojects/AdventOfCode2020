package day1

import base.UnitSpecBase

class AccountCheckerSpec extends UnitSpecBase {

  val testAccChecker = new AccountChecker

  "checkSumOfTwoNumbers" should "return true if 2 numbers add up to 2020, otherwise return false" in {
    testAccChecker.checkSumOfTwoNumbers(2019, 1) shouldBe true
    testAccChecker.checkSumOfTwoNumbers(2019, 0) shouldBe false
  }

  "checkListOfNumbersTwoNums" should "return a Some containing the product of the two numbers that add up to 2020 (if they exist)" in {
    val inputList = List(2000, 10, 20, 40)
    testAccChecker.checkListOfNumbersTwoNums(inputList) shouldBe Some(40000)
  }

  "checkListOfNumbersTwoNums" should "return a None if there are no number combinations that add up to 2020" in {
    val inputList = List(2000, 10, 30, 40)
    testAccChecker.checkListOfNumbersTwoNums(inputList) shouldBe None
  }

  "checkListOfNumbersThreeNums" should "return a Some containing the product of the three numbers that add up to 2020 (if they exist)" in {
    val inputList = List(2016, 3, 1, 12, 42, 65)
    testAccChecker.checkListOfNumbersThreeNums(inputList) shouldBe Some(6048)
  }

  "checkListOfNumbersThreeNums" should "return a None if there are no number combinations that add up to 2020" in {
    val inputList = List(16, 3, 1, 12, 42, 65)
    testAccChecker.checkListOfNumbersThreeNums(inputList) shouldBe None
  }
}
