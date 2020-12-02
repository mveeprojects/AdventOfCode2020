package day2

import base.UnitSpecBase

class PasswordValidatorSpec extends UnitSpecBase {

  val testPassValidator = new PasswordValidator

  "lineRegexChecker" should "return Some(value) when input matches regex" in {
    val inputLine = "9-10 p: lmpsbqgzpxggltl"
    testPassValidator.lineRegexChecker(inputLine) shouldBe Some(inputLine)
  }

  "lineRegexChecker" should "return None when input does not match regex" in {
    val inputLine = "-1 a: abc"
    testPassValidator.lineRegexChecker(inputLine) shouldBe None
  }

  "lineSplitter" should "correctly separate the password policy from the password on a given input" in {
    val inputLine = "1-3 a: abcde"
    testPassValidator.lineSplitter(inputLine) shouldBe Right(PasswordWithPolicy(Policy("1-3", "a"), "abcde"))
  }

  "lineSplitter" should "return an error message is the policy : password format is incorrect (cannot split)" in {
    val inputLine = "1-3 a abcde"
    testPassValidator.lineSplitter(inputLine) shouldBe Left(PasswordValidatorError("badly formatted input"))
  }
}
