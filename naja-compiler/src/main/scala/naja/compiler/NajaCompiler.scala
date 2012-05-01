package naja.compiler

import java.io.File

object NajaCompiler {
  def compile(templateFile: File, sourceDir: File, generatedDir: File) = {
    val generatedFile = new File(generatedDir, "foo.scala")
    val generatedSource = "object foo { def test() = 4 }\n"

    (generatedFile, generatedSource)
  }
}
