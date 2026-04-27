import java.io.File
import scala.io.StdIn

object VMTranslator {

  def main(args: Array[String]): Unit = {

    val filePath =
      if (args.length == 1) {
        args(0)
      } else {
        println("Enter path to .vm file:")
        StdIn.readLine()
      }

    val inputFile = new File(filePath)

    if (!inputFile.exists()) {
      println(s"File not found: $filePath")
      System.exit(1)
    }

    val outputFile =
      new File(inputFile.getAbsolutePath.replace(".vm", ".asm"))

    val parser = new Parser(inputFile)
    val codeWriter = new CodeWriter(outputFile, inputFile.getName.replace(".vm", ""))


    while (parser.hasMoreCommands) {
      parser.advance()

      codeWriter.writeComment(parser.currentCommandString)

      parser.commandType match {
        //STAGE 1
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

    codeWriter.close()
    println("Translation completed.")
  }
}