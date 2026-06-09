import java.io.PrintWriter

// Writes VM commands to a PrintWriter
class VMWriter(output: PrintWriter) {

  def writePush(segment: String, index: Int): Unit =
    output.println(s"push $segment $index")

  def writePop(segment: String, index: Int): Unit =
    output.println(s"pop $segment $index")

  def writeArithmetic(command: String): Unit =
    output.println(command)

  def writeLabel(label: String): Unit =
    output.println(s"label $label")

  def writeGoto(label: String): Unit =
    output.println(s"goto $label")

  def writeIf(label: String): Unit =
    output.println(s"if-goto $label")

  def writeCall(name: String, nArgs: Int): Unit =
    output.println(s"call $name $nArgs")

  def writeFunction(name: String, nLocals: Int): Unit =
    output.println(s"function $name $nLocals")

  def writeReturn(): Unit =
    output.println("return")
}
