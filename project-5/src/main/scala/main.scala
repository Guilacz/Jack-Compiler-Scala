import scala.io.{Source, StdIn}
import java.io.{File, PrintWriter}

object Main {

  def main(args: Array[String]): Unit = {

    val path =
      if (args.length == 1) args(0)
      else {
        println("Enter path to .jack file or directory:")
        StdIn.readLine()
      }

    val inputFile = new File(path)

    // accept file.jack or file that contains .jack
    val jackFiles: List[File] =
      if (inputFile.isDirectory)
        inputFile.listFiles().toList.filter(_.getName.endsWith(".jack"))
      else
        List(inputFile)

    if (jackFiles.isEmpty) {
      println("No .jack files found.")
      return
    }

    jackFiles.foreach { file =>
      compileFile(file)
    }
  }

  def compileFile(inputFile: File): Unit = {
    val inputCode = Source.fromFile(inputFile).mkString

    // Tokenize
    val tokenizer = new JackTokenizer(inputCode)
    val tokens    = tokenizer.tokenize()

    val projectFolderName = inputFile.getParentFile.getName
    val baseName          = inputFile.getName.stripSuffix(".jack")

    // output file
    val outputDir = new File(s"Outputs/outputs stage 5/$projectFolderName")
    if (!outputDir.exists()) outputDir.mkdirs()

    // output vm
    val vmFile = new File(outputDir, s"$baseName.vm")
    val vmOut  = new PrintWriter(vmFile)

    val engine = new CompilationEngine(tokens, vmOut)
    engine.compileClass()

    vmOut.close()

    println(s"VM DONE -> ${vmFile.getPath}")
  }
}
