package day1

import base.UnitSpecBase
import day1.AccountChecker.checkNumbers

class AccountCheckerSpec extends UnitSpecBase {

  behavior of "checkNumbers"

  it should "return true if 2 numbers add up to 2020, otherwise return false" in {
    checkNumbers(2019, 1) shouldBe true
    checkNumbers(2019, 0) shouldBe false
  }

}
