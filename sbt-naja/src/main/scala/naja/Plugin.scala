package naja

import naja.compiler.NajaCompiler
import sbt._
import sbt.Keys._

object SbtNajaCompiler {
  def compile(sourceDir: File, generatedDir: File, streams: TaskStreams) = {
    IO.createDirectory(generatedDir)
    // cleanUp(generatedDir)

    val templates = collectTemplates(sourceDir)
    streams.log.info("naja: Preparing " + templates.size + " template(s) ...")

    templates map { templateFile =>
      val skipChars = sourceDir.toString.length
      streams.log.info(
        "Compiling naja template ..." + templateFile.toString.substring(skipChars) + " to .../" /*+ targetFile.getName*/
      )

      val (generatedFile, generatedSource) = NajaCompiler.compile(templateFile, sourceDir, generatedDir)

      IO.write(generatedFile, generatedSource)
      generatedFile
    }
  }

  def collectTemplates(sourceDir: File) = {
    (sourceDir ** "*.haml").get
  }

  // def cleanUp(dir: File){
  //   (dir ** "*.naja.scala").get.foreach {
  //     GeneratedSource(_).sync()
  //   }
  // }
}

trait NajaKeys {
  val najaCompile = TaskKey[Seq[File]]("naja-compile", "Compile haml files")
}

object NajaPlugin extends Plugin {
  object Naja extends NajaKeys {
    def settings = seq(
      sourceDirectory in najaCompile <<= (sourceDirectory in Compile) / "naja",
      target in najaCompile <<= (sourceManaged in Compile) / "naja-generated",

      najaCompile <<= (
        sourceDirectory in najaCompile,
        target in najaCompile,
        streams
      ) map SbtNajaCompiler.compile,

      (sourceGenerators in Compile) <+= najaCompile,
      (managedSourceDirectories in Compile) <+= target in najaCompile,
      (compile in  Compile) <<= (compile in Compile).dependsOn(najaCompile)
    )
  }
}
