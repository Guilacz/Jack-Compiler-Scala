import scala.io.{Source, StdIn}
import java.io.File
import java.io.PrintWriter

object Main {

  def main(args: Array[String]): Unit = {

    val path =
      if (args.length == 1) args(0)
      else {
        println("Enter path to .jack file:")
        StdIn.readLine()
      }

    val inputFile = new File(path)
    val inputCode = Source.fromFile(inputFile).mkString

    val tokenizer = new JackTokenizer(inputCode)
    val tokens = tokenizer.tokenize()

    val projectFolderName = inputFile.getParentFile.getName

    val outputDir = new File(s"Outputs/outputs stage 4/$projectFolderName")

    if (!outputDir.exists()) outputDir.mkdirs()

    val fileName = inputFile.getName.replace(".jack", "T.xml")

    val outputFile = new File(outputDir, fileName)
    val outPath = outputFile.getPath

    val out = new PrintWriter(outputFile)
    val engine = new CompilationEngine(tokens, out)
    engine.compileClass()

    out.close()

    println("DONE -> " + outPath)
  }
}