package day2

import base.UnitSpecBase

class PasswordValidatorSpec extends UnitSpecBase {

  private val testPassValidator = new PasswordValidator
  private val goodInputPolicyOccString = "9-10"
  private val goodInputPolicyLetter = "p:"
  private val goodInputPolicyLetterAfterProcessing = 'p'
  private val goodInputPassword = "lmasbqgzppppppppp"
  private val badInputPassword = "lmpsbqgzpxggltl"
  private val goodInput = s"$goodInputPolicyOccString $goodInputPolicyLetter $goodInputPassword"
  private val goodPasswordPolicy = PasswordWithPolicy(Policy(goodInputPolicyOccString, goodInputPolicyLetterAfterProcessing), goodInputPassword)
  private val badPasswordPolicy = PasswordWithPolicy(Policy(goodInputPolicyOccString, goodInputPolicyLetterAfterProcessing), badInputPassword)
  private val badInput = goodInput.replace(goodInputPolicyOccString, "")

  private val goodPositionLetterInput = PasswordWithPolicy(Policy("1-3", 'm'), "mark")
  private val badTwoPositionLetterInput = PasswordWithPolicy(Policy("1-3", 'm'), "mamk")
  private val badNonExistentPositionLetterInput = PasswordWithPolicy(Policy("1-3", 'm'), "aaak")

  "lineRegexChecker" should "return Some(value) when input matches regex" in {
    testPassValidator.lineRegexChecker(goodInput) shouldBe Some(goodInput)
  }

  "lineRegexChecker" should "return None when input does not match regex" in {
    testPassValidator.lineRegexChecker(badInput) shouldBe None
  }

  "lineSplitter" should "correctly separate the password policy from the password on a given input" in {
    testPassValidator.lineSplitter(goodInput) shouldBe Right(PasswordWithPolicy(Policy(goodInputPolicyOccString, goodInputPolicyLetterAfterProcessing), goodInputPassword))
  }

  "lineSplitter" should "return an error message is the policy : password format is incorrect (cannot split)" in {
    testPassValidator.lineSplitter(badInput) shouldBe Left(PasswordValidationError("badly formatted input"))
  }

  "stringToPasswordWithPolicy" should "return the original string if it matches the regex pattern" in {
    testPassValidator.stringToPasswordWithPolicy(goodInput) shouldBe goodPasswordPolicy
  }

  "checkPasswordAgainstPolicy" should "return true if the password meets the policy" in {
    testPassValidator.checkPasswordAgainstPolicy(goodPasswordPolicy) shouldBe true
  }

  "checkPasswordAgainstPolicy" should "return false if the password does not meet the policy" in {
    testPassValidator.checkPasswordAgainstPolicy(badPasswordPolicy) shouldBe false
  }

  "checkPasswordLetterPositioning" should "return true if the required letter only turns up in one of the specified positions" in {
    testPassValidator.checkPasswordLetterPositioning(goodPositionLetterInput) shouldBe true
  }

  "checkPasswordLetterPositioning" should "return false if the required letter turns up in both of the specified positions" in {
    testPassValidator.checkPasswordLetterPositioning(badTwoPositionLetterInput) shouldBe false
  }

  "checkPasswordLetterPositioning" should "return false if the required letter turns up in neither of the specified positions" in {
    testPassValidator.checkPasswordLetterPositioning(badNonExistentPositionLetterInput) shouldBe false
  }
}
