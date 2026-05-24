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
    // OUTPUT FOLDER (Mis à jour pour : outputs stage 4 part 2)
    // =========================
    val outputDir = new File(s"Outputs/outputs stage 4 part 2/$projectFolderName")
    if (!outputDir.exists()) outputDir.mkdirs()

    // Extraction du nom de base sans l'extension .jack (ex: "Main" ou "array")
    val baseName = inputFile.getName.stripSuffix(".jack")

    // =========================
    // 1. TOKENIZER OUTPUT (ex: MainT.xml ou arrayT.xml)
    // =========================
    val tokenizerFileName = s"${baseName}T.xml"
    val tokenizerFile = new File(outputDir, tokenizerFileName)
    val tokenizerOut = new PrintWriter(tokenizerFile)

    val tokenizerXML = tokenizer.toXML(tokens)

    tokenizerOut.write(tokenizerXML)
    tokenizerOut.close()

    println("TOKENIZER DONE -> " + tokenizerFile.getPath)

    // =========================
    // 2. PARSER OUTPUT (ex: Main.xml ou array.xml)
    // =========================
    val parserFileName = s"$baseName.xml"
    val parserFile = new File(outputDir, parserFileName)
    val parserOut = new PrintWriter(parserFile)

    val engine = new CompilationEngine(tokens, parserOut)
    engine.compileClass()

    parserOut.close()

    println("PARSER DONE -> " + parserFile.getPath)
  }
}