import java.io.PrintWriter

class CompilationEngine(tokens: List[Token], output: PrintWriter) {

  var i = 0

  def token: Token = tokens(i)

  def advance(): Unit =
    i += 1

  var indentLevel = 0

  def indent(): String =
    "  " * indentLevel

  def write(line: String): Unit =
    output.println(indent() + line)

  def openTag(tag: String): Unit = {
    write(s"<$tag>")
    indentLevel += 1
  }

  def closeTag(tag: String): Unit = {
    indentLevel -= 1
    write(s"</$tag>")
  }

  def printToken(): Unit = token match {
    case Keyword(v)    => write(s"<keyword> $v </keyword>")
    case Symbol(v)     => write(s"<symbol> $v </symbol>")
    case Identifier(v) => write(s"<identifier> $v </identifier>")
    case IntConst(v)   => write(s"<integerConstant> $v </integerConstant>")
    case StringConst(v)=> write(s"<stringConstant> $v </stringConstant>")
  }

  def compileClass(): Unit = {

    openTag("<class>")

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

    while (token match {
      case Keyword("constructor") => true
      case Keyword("function") => true
      case Keyword("method") => true
      case _ => false
    }) {
      compileSubroutine()
    }

    printToken()
    advance() // }

    closeTag("</class>")
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

    openTag("<classVarDec>")

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

    closeTag("</classVarDec>")
  }

  // Compiles a subroutine declaration.
  // Grammar:
  // ('constructor' | 'function' | 'method')
  // ('void' | type)
  // subroutineName
  // '(' parameterList ')'
  // subroutineBody
  def compileSubroutine(): Unit = {

    openTag("<subroutineDec>")

    // constructor | function | method
    printToken()
    advance()

    // return type (void | int | boolean | className)
    printToken()
    advance()

    // subroutine name
    printToken()
    advance()

    // '('
    printToken()
    advance()

    // parameter list
    openTag("<parameterList>")
    // (to implement later: compileParameterList)
    closeTag("</parameterList>")

    // ')'
    printToken()
    advance()

    // subroutine body
    compileSubroutineBody()

    closeTag("</subroutineDec>")
  }

  // Compiles a subroutine body.
  // Grammar:
  // '{' varDec* statements '}'
  def compileSubroutineBody(): Unit = {

    openTag("<subroutineBody>")

    // '{'
    printToken()
    advance()

    // var declarations (local variables)
    while (token match {
      case Keyword("var") => true
      case _ => false
    }) {
      compileVarDec()
    }

    // statements (let, if, while, do, return)
    openTag("<statements>")
    compileStatements()
    closeTag("</statements>")

    // '}'
    printToken()
    advance()

    closeTag("</subroutineBody>")
  }

  // Compiles a variable declaration inside a subroutine.
  // Grammar: 'var' type varName (',' varName)* ';'
  def compileVarDec(): Unit = {

    openTag("<varDec>")

    // 'var'
    printToken()
    advance()

    // type
    printToken()
    advance()

    // varName
    printToken()
    advance()

    // (',' varName)*
    while (token match {
      case Symbol(",") => true
      case _ => false
    }) {
      printToken() // ,
      advance()

      printToken() // varName
      advance()
    }

    // ';'
    printToken()
    advance()

    closeTag("</varDec>")
  }

  // Compiles a sequence of statements.
  // Grammar:
  // letStatement | ifStatement | whileStatement | doStatement | returnStatement
  def compileStatements(): Unit = {

    while (token match {
      case Keyword("let") => true
      case Keyword("if") => true
      case Keyword("while") => true
      case Keyword("do") => true
      case Keyword("return") => true
      case _ => false
    }) {

      token match {

        case Keyword("let") =>
          compileLet()

        case Keyword("if") =>
          compileIf()

        case Keyword("while") =>
          compileWhile()

        case Keyword("do") =>
          compileDo()

        case Keyword("return") =>
          compileReturn()

        case _ =>
      }
    }
  }

  // Compiles a let statement.
  // Grammar:
  // 'let' varName ('[' expression ']')? '=' expression ';'
  def compileLet(): Unit = {

    openTag("<letStatement>")

    // 'let'
    printToken()
    advance()

    // varName
    printToken()
    advance()

    // optional array access: [expression]
    if (token match {
      case Symbol("[") => true
      case _ => false
    }) {

      // '['
      printToken()
      advance()

      // expression inside brackets
      compileExpression()

      // ']'
      printToken()
      advance()
    }

    // '='
    printToken()
    advance()

    // expression (right side)
    compileExpression()

    // ';'
    printToken()
    advance()

    closeTag("</letStatement>")
  }

  // Compiles a do statement.
  // Grammar:
  // 'do' subroutineCall ';'
  def compileDo(): Unit = {

    openTag("<doStatement>")

    // 'do'
    printToken()
    advance()

    // subroutine call
    compileSubroutineCall()

    // ';'
    printToken()
    advance()

    closeTag("</doStatement>")
  }

  // Compiles a subroutine call:
  // functionName() | className.methodName() | varName.methodName()
  def compileSubroutineCall(): Unit = {

    // first identifier already printed in term OR caller context
    // BUT here we assume we are AFTER identifier

    if (tokens(i - 1) match {
      case Identifier(_) => true
      case _ => false
    }) {
      // already consumed
    }

    // '.' method call
    if (token match {
      case Symbol(".") => true
      case _ => false
    }) {
      printToken()
      advance()

      printToken()
      advance()
    }

    // '('
    printToken()
    advance()

    compileExpressionList()

    // ')'
    printToken()
    advance()
  }

  // Compiles a return statement.
  // Grammar:
  // 'return' expression? ';'
  def compileReturn(): Unit = {

    openTag("<returnStatement>")

    // 'return'
    printToken()
    advance()

    // optional expression
    token match {
      case Symbol(";") =>
      // return void

      case _ =>
        // return expression
        compileExpression()
    }

    // ';'
    printToken()
    advance()

    closeTag("</returnStatement>")
  }

  // Compiles a while statement.
  // Grammar:
  // 'while' '(' expression ')' '{' statements '}'
  def compileWhile(): Unit = {

    openTag("<whileStatement>")

    // 'while'
    printToken()
    advance()

    // '('
    printToken()
    advance()

    // condition expression
    compileExpression()

    // ')'
    printToken()
    advance()

    // '{'
    printToken()
    advance()

    // statements inside loop
    openTag("<statements>")
    compileStatements()
    closeTag("</statements>")

    // '}'
    printToken()
    advance()

    closeTag("</whileStatement>")
  }

  // Compiles an if statement.
  // Grammar:
  // 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
  def compileIf(): Unit = {

    openTag("<ifStatement>")

    // 'if'
    printToken()
    advance()

    // '('
    printToken()
    advance()

    // condition
    compileExpression()

    // ')'
    printToken()
    advance()

    // '{'
    printToken()
    advance()

    // if statements
    openTag("<statements>")
    compileStatements()
    closeTag("</statements>")

    // '}'
    printToken()
    advance()

    // optional else
    if (token match {
      case Keyword("else") => true
      case _ => false
    }) {

      // 'else'
      printToken()
      advance()

      // '{'
      printToken()
      advance()

      // else statements
      openTag("<statements>")
      compileStatements()
      closeTag("</statements>")

      // '}'
      printToken()
      advance()
    }

    closeTag("</ifStatement>")
  }

  // Compiles an expression.
  // Grammar:
  // term (op term)*
  def compileExpression(): Unit = {

    openTag("<expression>")

    // first term
    compileTerm()

    // (op term)*
    while (token match {
      case Symbol("+") | Symbol("-") | Symbol("*") |
           Symbol("/") | Symbol("&") | Symbol("|") |
           Symbol("<") | Symbol(">") | Symbol("=") => true
      case _ => false
    }) {

      // operator
      printToken()
      advance()

      // next term
      compileTerm()
    }

    closeTag("</expression>")
  }

  // Compiles a term.
  // A term is a basic unit of an expression.
  def compileTerm(): Unit = {

    openTag("<term>")

    token match {

      // integer
      case IntConst(_) =>
        printToken()
        advance()

      // string
      case StringConst(_) =>
        printToken()
        advance()

      // keyword constants: true, false, null, this
      case Keyword("true") | Keyword("false") |
           Keyword("null") | Keyword("this") =>
        printToken()
        advance()

      // (expression)
      case Symbol("(") =>
        printToken()
        advance()
        compileExpression()
        printToken() // ')'
        advance()

      // unary operators
      case Symbol("-") | Symbol("~") =>
        printToken()
        advance()
        compileTerm()

      case Identifier(_) =>
        printToken()
        advance()

        token match {

          // array access
          case Symbol("[") =>
            printToken()
            advance()
            compileExpression()
            printToken()
            advance()

          // subroutine call
          case Symbol("(") | Symbol(".") =>
            compileSubroutineCall()

          case _ =>
        }

    closeTag("</term>")
  }
  }

 // Compiles a list of expressions.
  // Grammar:
  // (expression (',' expression)*)?
  def compileExpressionList(): Unit = {

    openTag("<expressionList>")

    // empty list case
    if (token match {
      case Symbol(")") => true
      case _ => false
    }) {
      // nothing inside
    }
    else {

      // first expression
      compileExpression()

      // (',' expression)*
      while (token match {
        case Symbol(",") => true
        case _ => false
      }) {

        // ','
        printToken()
        advance()

        // next expression
        compileExpression()
      }
    }

    closeTag("</expressionList>")
  }

}