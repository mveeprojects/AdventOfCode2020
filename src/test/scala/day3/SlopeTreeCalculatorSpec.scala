package day3

import base.UnitSpecBase

class SlopeTreeCalculatorSpec extends UnitSpecBase {

  val testSlopeTreeCalculator = new SlopeTreeCalculator

  "checkIfTree" should "return true if tree (#)" in {
    testSlopeTreeCalculator.checkIfTree('#') shouldBe true
    testSlopeTreeCalculator.checkIfTree('.') shouldBe false
  }

  "calculateNextColumnPosition" should "move the Toboggan 3 to the right and 1 down" in {
    val tobogganPosition: TobogganPosition = TobogganPosition(0, 0)
    testSlopeTreeCalculator.calculateNextColumnPosition(tobogganPosition) shouldBe 3
  }

  "calculateNextColumnPosition" should "move the Toboggan 3 to the right and 1 down accounting for the edge of the array" in {
    val tobogganPosition: TobogganPosition = TobogganPosition(30, 0)
    testSlopeTreeCalculator.calculateNextColumnPosition(tobogganPosition) shouldBe 3
  }
}
