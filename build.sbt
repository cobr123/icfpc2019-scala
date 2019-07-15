name := "icfpc2019-scala"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.0.8" % "test"

lazy val app = (project in file(".")).
  settings(
    version := "0.1-SNAPSHOT",
    organization := "com.example",
    scalaVersion := "2.13.0",
    test in assembly := {},
    mainClass in assembly := Some("com.example.Main")
  )