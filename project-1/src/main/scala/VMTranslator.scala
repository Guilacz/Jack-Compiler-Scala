import java.io.File
import scala.io.StdIn

object VMTranslator {

  def main(args: Array[String]): Unit = {

    val path =
      if (args.length == 1) {
        args(0)
      } else {
        println("Enter path to .vm file or folder:")
        StdIn.readLine()
      }

    val input = new File(path)

    if (!input.exists()) {
      println(s"Path not found: $path")
      System.exit(1)
    }

    // If input is a folder → output file = FolderName.asm inside folder
    // If input is a file   → output file = SameName.asm
    val outputFile =
      if (input.isDirectory)
        new File(input, input.getName + ".asm")
      else
        new File(input.getAbsolutePath.replace(".vm", ".asm"))

    val codeWriter = new CodeWriter(outputFile)

    // Bootstrap code
    codeWriter.writeInit()

    // Get all .vm files
    val vmFiles =
      if (input.isDirectory)
        input.listFiles().filter(f => f.isFile && f.getName.endsWith(".vm")).toList
      else
        List(input)

    // Translate each file
    for (file <- vmFiles) {

      val parser = new Parser(file)

      codeWriter.setFileName(file.getName.replace(".vm", ""))

      while (parser.hasMoreCommands) {
        parser.advance()

        codeWriter.writeComment(parser.currentCommandString)

        parser.commandType match {

          // STAGE 1
          case C_ARITHMETIC =>
            codeWriter.writeArithmetic(parser.arg1.get)

          case C_PUSH =>
            codeWriter.writePushPop("push", parser.arg1.get, parser.arg2.get)

          case C_POP =>
            codeWriter.writePushPop("pop", parser.arg1.get, parser.arg2.get)

          // STAGE 2
          case C_LABEL =>
            codeWriter.writeLabel(parser.arg1.get)

          case C_GOTO =>
            codeWriter.writeGoto(parser.arg1.get)

          case C_IF =>
            codeWriter.writeIf(parser.arg1.get)

          case C_FUNCTION =>
            codeWriter.writeFunction(parser.arg1.get, parser.arg2.get)

          case C_CALL =>
            codeWriter.writeCall(parser.arg1.get, parser.arg2.get)

          case C_RETURN =>
            codeWriter.writeReturn()
        }
      }
    }

    codeWriter.close()
    println("Translation completed.")
  }
}