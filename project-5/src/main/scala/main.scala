import scala.io.{Source, StdIn}
import java.io.{File, PrintWriter}

object main {

  def main(args: Array[String]): Unit = {

    val path =
      if (args.length == 1) args(0)
      else {
        println("Enter path to .jack file or directory:")
        StdIn.readLine()
      }

    val inputFile = new File(path)

    val jackFiles: List[File] =
      if (inputFile.isDirectory)
        inputFile.listFiles().toList.filter(_.getName.endsWith(".jack"))
      else
        List(inputFile)

    if (jackFiles.isEmpty) {
      println("No .jack files found.")
      return
    }

    println(s"Found ${jackFiles.length} .jack file(s):")
    jackFiles.foreach(f => println(s"  - ${f.getName}"))
    println()

    jackFiles.foreach(compileFile)
  }

  def compileFile(inputFile: File): Unit = {
    val inputCode = Source.fromFile(inputFile).mkString

    val tokenizer = new JackTokenizer(inputCode)
    val tokens = tokenizer.tokenize()

    val projectFolderName = inputFile.getParentFile.getName
    val baseName = inputFile.getName.stripSuffix(".jack")

    val vmFileSide = new File(inputFile.getParentFile, s"$baseName.vm")

    val outputDir = new File(s"Outputs/outputs stage 5/$projectFolderName")
    if (!outputDir.exists()) outputDir.mkdirs()
    val vmFileOut = new File(outputDir, s"$baseName.vm")

    // written in both at the same time
    val writerSide = new PrintWriter(vmFileSide)
    val writerOut = new PrintWriter(vmFileOut)
    val vmOut = new PrintWriter(writerSide) {
      override def println(x: String): Unit = {
        writerSide.println(x)
        writerOut.println(x)
      }

      override def close(): Unit = {
        writerSide.close()
        writerOut.close()
      }
    }

    try {
      val engine = new CompilationEngine(tokens, vmOut)
      engine.compileClass()
      println(s"${inputFile.getName}")
      println(s"    -> ${vmFileSide.getPath}")
      println(s"    -> ${vmFileOut.getPath}")
    } catch {
      case e: Exception =>
        println(s"✗ ${inputFile.getName} : ERROR - ${e.getMessage}")
    } finally {
      vmOut.close()
    }
  }
}
