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

    // =========================
    // TOKENIZER
    // =========================
    val tokenizer = new JackTokenizer(inputCode)
    val tokens = tokenizer.tokenize()

    val projectFolderName = inputFile.getParentFile.getName

    // =========================
    // OUTPUT FOLDER
    // =========================
    val outputDir = new File(s"Outputs/$projectFolderName")
    if (!outputDir.exists()) outputDir.mkdirs()

    // =========================
    // 1. TOKENIZER OUTPUT (T.XML)
    // =========================
    val tokenizerFileName =
      inputFile.getName.replace(".jack", "T.xml")

    val tokenizerFile = new File(outputDir, tokenizerFileName)
    val tokenizerOut = new PrintWriter(tokenizerFile)

    val tokenizerXML = tokenizer.toXML(tokens)

    tokenizerOut.write(tokenizerXML)
    tokenizerOut.close()

    println("TOKENIZER DONE -> " + tokenizerFile.getPath)

    // =========================
    // 2. PARSER OUTPUT (.XML)
    // =========================
    val parserFileName =
      inputFile.getName.replace(".jack", ".xml")

    val parserFile = new File(outputDir, parserFileName)
    val parserOut = new PrintWriter(parserFile)

    val engine = new CompilationEngine(tokens, parserOut)
    engine.compileClass()

    parserOut.close()

    println("PARSER DONE -> " + parserFile.getPath)
  }
}