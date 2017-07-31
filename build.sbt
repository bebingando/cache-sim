import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.bebingando",
      scalaVersion := "2.11.6",
      version      := "0.1.0-SNAPSHOT",
      cancelable in Global := true
    )),
    name := "Cache Sim",
    libraryDependencies ++= Seq(
      scalaTest % Test
    )
  )
