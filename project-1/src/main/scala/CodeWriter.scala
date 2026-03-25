import java.io._

class CodeWriter(outputFile: File, fileName: String) {

  private val writer = new BufferedWriter(new FileWriter(outputFile))
  private var labelCounter = 0

  private def writeLine(s: String): Unit = {
    writer.write(s)
    writer.newLine()
  }

  def close(): Unit = writer.close()

  // -----------------------------
  // Arithmetic
  // -----------------------------

  def writeArithmetic(command: String): Unit = {

    command match {

      case "add" | "sub" | "and" | "or" =>
        writeBinary(command)

      case "neg" | "not" =>
        writeUnary(command)

      case "eq" =>
        writeComparison("JEQ")

      case "gt" =>
        writeComparison("JGT")

      case "lt" =>
        writeComparison("JLT")
    }
  }

  private def writeBinary(op: String): Unit = {
    writeLine("@SP")
    writeLine("AM=M-1")
    writeLine("D=M")
    writeLine("A=A-1")

    op match {
      case "add" => writeLine("M=M+D")
      case "sub" => writeLine("M=M-D")
      case "and" => writeLine("M=M&D")
      case "or"  => writeLine("M=M|D")
    }
  }

  private def writeUnary(op: String): Unit = {
    writeLine("@SP")
    writeLine("A=M-1")

    op match {
      case "neg" => writeLine("M=-M")
      case "not" => writeLine("M=!M")
    }
  }

  private def writeComparison(jump: String): Unit = {
    val trueLabel = s"TRUE_$labelCounter"
    val endLabel = s"END_$labelCounter"
    labelCounter += 1

    writeLine("@SP")
    writeLine("AM=M-1")
    writeLine("D=M")
    writeLine("A=A-1")
    writeLine("D=M-D")

    writeLine(s"@$trueLabel")
    writeLine(s"D;$jump")

    writeLine("@SP")
    writeLine("A=M-1")
    writeLine("M=0")

    writeLine(s"@$endLabel")
    writeLine("0;JMP")

    writeLine(s"($trueLabel)")
    writeLine("@SP")
    writeLine("A=M-1")
    writeLine("M=-1")

    writeLine(s"($endLabel)")
  }

  // -----------------------------
  // Push / Pop
  // -----------------------------

  def writePushPop(command: String, segment: String, index: Int): Unit = {

    command match {
      case "push" => writePush(segment, index)
      case "pop"  => writePop(segment, index)
    }
  }

  private def writePush(segment: String, index: Int): Unit = {

    segment match {

      case "constant" =>
        writeLine(s"@$index")
        writeLine("D=A")

      case "local" | "argument" | "this" | "that" =>
        val base = segment match {
          case "local"    => "LCL"
          case "argument" => "ARG"
          case "this"     => "THIS"
          case "that"     => "THAT"
        }
        writeLine(s"@$base")
        writeLine("D=M")
        writeLine(s"@$index")
        writeLine("A=D+A")
        writeLine("D=M")

      case "temp" =>
        writeLine(s"@${5 + index}")
        writeLine("D=M")

      case "pointer" =>
        val ptr = if (index == 0) "THIS" else "THAT"
        writeLine(s"@$ptr")
        writeLine("D=M")

      case "static" =>
        writeLine(s"@$fileName.$index")
        writeLine("D=M")
    }

    writeLine("@SP")
    writeLine("A=M")
    writeLine("M=D")
    writeLine("@SP")
    writeLine("M=M+1")
  }

  private def writePop(segment: String, index: Int): Unit = {

    segment match {

      case "local" | "argument" | "this" | "that" =>
        val base = segment match {
          case "local"    => "LCL"
          case "argument" => "ARG"
          case "this"     => "THIS"
          case "that"     => "THAT"
        }

        writeLine(s"@$base")
        writeLine("D=M")
        writeLine(s"@$index")
        writeLine("D=D+A")
        writeLine("@R13")
        writeLine("M=D")

        writeLine("@SP")
        writeLine("AM=M-1")
        writeLine("D=M")

        writeLine("@R13")
        writeLine("A=M")
        writeLine("M=D")

      case "temp" =>
        writeLine("@SP")
        writeLine("AM=M-1")
        writeLine("D=M")
        writeLine(s"@${5 + index}")
        writeLine("M=D")

      case "pointer" =>
        val ptr = if (index == 0) "THIS" else "THAT"
        writeLine("@SP")
        writeLine("AM=M-1")
        writeLine("D=M")
        writeLine(s"@$ptr")
        writeLine("M=D")

      case "static" =>
        writeLine("@SP")
        writeLine("AM=M-1")
        writeLine("D=M")
        writeLine(s"@$fileName.$index")
        writeLine("M=D")

      case _ =>
    }
  }

  def writeComment(comment: String): Unit = {
    writeLine(s"// $comment")
  }
}