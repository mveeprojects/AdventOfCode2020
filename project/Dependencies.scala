import sbt._

object Dependencies {

  object Version {
    val scalaTest = "3.2.2"
  }

  lazy val testDependencies: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % Version.scalaTest
  ).map(_ % Test)

  lazy val appDependencies: Seq[ModuleID] = testDependencies
}
