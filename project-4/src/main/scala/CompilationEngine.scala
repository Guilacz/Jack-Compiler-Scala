import java.io.PrintWriter

class CompilationEngine(tokens: List[Token], output: PrintWriter) {

  var i = 0

  def token: Token = tokens(i)

  def advance(): Unit =
    i += 1

  def write(line: String): Unit =
    output.println(line)

  def printToken(): Unit = token match {
    case Keyword(v)    => write(s"<keyword> $v </keyword>")
    case Symbol(v)     => write(s"<symbol> $v </symbol>")
    case Identifier(v) => write(s"<identifier> $v </identifier>")
    case IntConst(v)   => write(s"<integerConstant> $v </integerConstant>")
    case StringConst(v)=> write(s"<stringConstant> $v </stringConstant>")
  }

  def compileClass(): Unit = {

    write("<class>")

    printToken() // class
    advance()

    printToken() // name
    advance()

    printToken() // {
    advance()

    printToken() // }
    advance()

    write("</class>")
  }
}