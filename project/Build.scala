import sbt._
import sbt.Keys._
import ScriptedPlugin._

object Build extends Build {
  lazy val common = seq(
    organization  := "eu.teamon",
    version       := "0.1.0-SNAPSHOT",
    scalaVersion  := "2.9.1",
    scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")
  )

  // dependencies
  val specs2 =  "org.specs2" %% "specs2" % "1.9" % "test"
  val treehugger = "com.eed3si9n" %% "treehugger" % "0.1.3"


  lazy val root =
    Project("naja", file("."))
      .settings(common:_*)
      .aggregate(najaCompiler, sbtNaja)

  lazy val najaCompiler =
    Project("naja-compiler", file("naja-compiler"))
      .settings(common:_*)
      .settings(
        libraryDependencies ++= Seq(specs2, treehugger)
      )

  lazy val sbtNaja =
    Project("sbt-naja", file("sbt-naja"))
      .settings(common:_*)
      .settings(scriptedSettings: _*)
      .settings(
        sbtPlugin := true,
        scriptedBufferLog := false
      )
      .dependsOn(najaCompiler)
}
