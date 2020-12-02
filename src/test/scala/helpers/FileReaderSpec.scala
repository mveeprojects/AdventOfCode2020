package helpers

import base.UnitSpecBase
import helpers.FileReader.readInputFile

class FileReaderSpec extends UnitSpecBase {
  "readReport" should "return a list of ints if file is formatted correctly" in {
    readInputFile("/day1/testinputgood", _.toInt) shouldBe Right(List(2000, 10, 20, 40))
  }

  "readReport" should "return an error message if file is not correctly formatted" in {
    readInputFile("/day1/testinputbad", _.toInt) shouldBe Left("Something went wrong, make sure the input file exists at the path specified and is formatted correctly")
  }
}
