import java.io._

class Parser(file: File) {

  private val lines = scala.io.Source.fromFile(file)
    .getLines()
    .map(_.split("//")(0).trim)
    .filter(_.nonEmpty)
    .toList

  private var currentIndex = -1
  private var currentCommand: String = ""


  def hasMoreCommands: Boolean =
    currentIndex + 1 < lines.length

  def advance(): Unit = {
    currentIndex += 1
    currentCommand = lines(currentIndex)
  }

  def commandType: CommandType = {
    if (currentCommand.startsWith("push")) C_PUSH
    else if (currentCommand.startsWith("pop")) C_POP
    else C_ARITHMETIC
  }

  def arg1: Option[String] = {
    val parts = currentCommand.split(" ")
    commandType match {
      case C_ARITHMETIC => Some(parts(0))
      case _ => Some(parts(1))
    }
  }

  def arg2: Option[Int] = {
    val parts = currentCommand.split(" ")
    if (parts.length == 3) Some(parts(2).toInt)
    else None
  }

  def currentCommandString: String = currentCommand
}