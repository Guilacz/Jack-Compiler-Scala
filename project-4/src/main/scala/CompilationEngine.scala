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

    printToken()
    advance() // class
    printToken()
    advance() // name
    printToken()
    advance() // {


    while (token match {
      case Keyword("static") => true
      case Keyword("field") => true
      case _ => false
    }) {
      compileClassVarDec()
    }

    printToken()
    advance() // }

    write("</class>")
  }


  // Compiles a Jack class.
  // Structure:
  // class keyword
  // class name
  // '{'
  // class variable declarations (static / field)
  // subroutine declarations (methods, functions, constructors)
  // '}'
  def compileClassVarDec(): Unit = {

    write("<classVarDec>")

    // static | field
    printToken()
    advance()

    // type (int, boolean, char, or className)
    printToken()
    advance()

    // varName
    printToken()
    advance()

    // (, varName)*
    while (token match {
      case Symbol(",") => true
      case _ => false
    }) {
      printToken() // ,
      advance()

      printToken() // varName
      advance()
    }

    // ;
    printToken()
    advance()

    write("</classVarDec>")
  }
}