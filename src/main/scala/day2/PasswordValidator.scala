package day2

import helpers.FileReader.readInputFile

import scala.util.matching.Regex

case class PasswordValidatorError(msg: String) extends Exception()

case class Policy(occurrenceString: String, letter: String)

case class PasswordWithPolicy(policy: Policy, password: String)

object Main extends App {
  val pwdValidator = new PasswordValidator
  pwdValidator.run()
}

class PasswordValidator {

  val defaultInputFilePath = "/day2/input"
  val pattern: Regex = "[1-9]{1}-[1-9]{1}\\s[a-z]{1}:\\s[a-z]{1,20}".r
  val stringLineToLowercase: String => String = _.toLowerCase

  def run(inputReportPath: String = defaultInputFilePath): Unit = {
    readInputFile(inputReportPath, stringLineToLowercase) match {
      case Right(report) => report.map(lineSplitter).foreach(println)
      case Left(errorMessage) => println(errorMessage)
    }
  }

  def lineSplitter(line: String): Either[PasswordValidatorError, PasswordWithPolicy] = {
    lineRegexChecker(line) match {
      case Some(value) => Right(stringToPasswordWithPolicy(value))
      case None => Left(PasswordValidatorError("badly formatted input"))
    }
  }

  def lineRegexChecker(line: String): Option[String] = {
    pattern.findFirstIn(line)
  }

  val stringToPasswordWithPolicy: String => PasswordWithPolicy = (input: String) => {
    val policyPasswordArr = input.split(":")
    val policyArr = policyPasswordArr(0).split(" ")
    PasswordWithPolicy(
      Policy(policyArr(0), policyArr(1)),
      policyPasswordArr(1).replace(" ", "")
    )
  }
}
