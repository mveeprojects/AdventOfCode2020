package helpers

import java.io.File

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

object FileReader {

  def readInputFile[B](inputReportPath: String, func: String => B): Either[String, List[B]] = {
    val file = new File(getClass.getResource(inputReportPath).getPath)
    val source: BufferedSource = Source.fromFile(file)
    Try(source.getLines.map(func).toList) match {
      case Success(value) =>
        source.close
        Right(value)
      case Failure(_) =>
        source.close
        Left(s"Something went wrong, make sure the input file exists at the path specified and is formatted correctly")
    }
  }
}
