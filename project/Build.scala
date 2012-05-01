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


  lazy val root =
    Project("naja", file("."))
      .settings(common:_*)
      .aggregate(najaCompiler, sbtNaja)

  lazy val najaCompiler =
    Project("naja-compiler", file("naja-compiler"))
      .settings(common:_*)
      .settings(
        libraryDependencies += specs2
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
