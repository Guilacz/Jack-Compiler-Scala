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

    val inputCode = Source.fromFile(path).mkString

    val tokenizer = new JackTokenizer(inputCode)
    val tokens = tokenizer.tokenize()

    val xml = tokenizer.toXML(tokens)

    val outputDir = new File("Outputs/outputs stage 4")
    if (!outputDir.exists()) outputDir.mkdirs()

    val fileName = new File(path).getName.replace(".jack", "T.xml")

    val outputFile = new File(outputDir, fileName)
    val outPath = outputFile.getPath

    val out = new PrintWriter(outputFile)
    out.write(xml)
    out.close()

    println("DONE -> " + outPath)
  }
}