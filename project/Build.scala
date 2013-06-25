import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "connect4"
  val appVersion      = "1.0-SNAPSHOT"
  val scalaVersion = "2.10.2"
  
  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "org.scalatest" % "scalatest_2.10" % "2.0.M5b" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test",
    "com.typesafe.akka" % "akka-testkit_2.10" % "2.1.4",
    "org.scala-lang" % "scala-compiler" % "2.10.2"    
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
