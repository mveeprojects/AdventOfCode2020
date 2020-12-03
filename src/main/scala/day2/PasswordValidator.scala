package day2

import helpers.FileReader.readInputFile

import scala.util.matching.Regex

case class PasswordValidationError(msg: String) extends Exception()

case class Policy(occurrenceString: String, letter: Char)

case class PasswordWithPolicy(policy: Policy, password: String)

object Main extends App {
  val pwdValidator = new PasswordValidator
  pwdValidator.run()
}

class PasswordValidator {

  val oldRule = false

  val defaultInputFilePath = "/day2/input"
  val pattern: Regex = "[0-99]{1,2}-[0-99]{1,2}\\s[a-z]{1}:\\s[a-z]{1,20}".r
  val stringLineToLowercase: String => String = _.toLowerCase

  def run(inputReportPath: String = defaultInputFilePath): Unit = {
    val result = readInputFile(inputReportPath, stringLineToLowercase) match {
      case Right(report) =>
        val res: List[PasswordWithPolicy] = report
          .map(lineSplitter)
          .map { case Right(value) => value }
        if (oldRule) res.count(checkPasswordAgainstPolicy)
        else res.count(checkPasswordLetterPositioning)
      case Left(errorMessage) => println(errorMessage)
    }
    println(result)
  }

  def lineSplitter(line: String): Either[PasswordValidationError, PasswordWithPolicy] = {
    lineRegexChecker(line) match {
      case Some(value) => Right(stringToPasswordWithPolicy(value))
      case None => Left(PasswordValidationError("badly formatted input"))
    }
  }

  def lineRegexChecker(line: String): Option[String] = {
    pattern.findFirstIn(line)
  }

  val stringToPasswordWithPolicy: String => PasswordWithPolicy = (input: String) => {
    val policyPasswordArr = input.split(":")
    val policyArr = policyPasswordArr(0).split(" ")
    PasswordWithPolicy(
      Policy(policyArr(0), policyArr(1).toCharArray.head),
      policyPasswordArr(1).replace(" ", "")
    )
  }

  def checkPasswordAgainstPolicy(passwordWithPolicy: PasswordWithPolicy): Boolean = {
    val occArr = passwordWithPolicy.policy.occurrenceString.split("-")
    val minOccurances = occArr(0).toInt
    val maxOccurances = occArr(1).toInt
    val actualOccurances = passwordWithPolicy.password
      .count(_ == passwordWithPolicy.policy.letter)
    if (minOccurances <= actualOccurances && actualOccurances <= maxOccurances) true
    else false
  }

  def checkPasswordLetterPositioning(passwordWithPolicy: PasswordWithPolicy): Boolean = {
    val occArr = passwordWithPolicy.policy.occurrenceString.split("-")
    val firstPossibleOccurance = occArr(0).toInt - 1
    val secondPossibleOccurance = occArr(1).toInt - 1
    val actualPositionOfOccurances = passwordWithPolicy.password
      .toList
      .zipWithIndex
      .collect { case (passwordWithPolicy.policy.letter, i) => i }
    if (actualPositionOfOccurances.contains(firstPossibleOccurance) && actualPositionOfOccurances.contains(secondPossibleOccurance)) false
    else if (actualPositionOfOccurances.contains(firstPossibleOccurance) || actualPositionOfOccurances.contains(secondPossibleOccurance)) true
    else false
  }
}
