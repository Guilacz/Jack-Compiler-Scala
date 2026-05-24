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

  // Compiles a subroutine declaration.
  // Grammar:
  // ('constructor' | 'function' | 'method')
  // ('void' | type)
  // subroutineName
  // '(' parameterList ')'
  // subroutineBody
  def compileSubroutine(): Unit = {

    write("<subroutineDec>")

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
    write("<parameterList>")
    // (to implement later: compileParameterList)
    write("</parameterList>")

    // ')'
    printToken()
    advance()

    // subroutine body
    compileSubroutineBody()

    write("</subroutineDec>")
  }

  // Compiles a subroutine body.
  // Grammar:
  // '{' varDec* statements '}'
  def compileSubroutineBody(): Unit = {

    write("<subroutineBody>")

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
    write("<statements>")
    compileStatements()
    write("</statements>")

    // '}'
    printToken()
    advance()

    write("</subroutineBody>")
  }

  // Compiles a variable declaration inside a subroutine.
  // Grammar: 'var' type varName (',' varName)* ';'
  def compileVarDec(): Unit = {

    write("<varDec>")

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

    write("</varDec>")
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

    write("<letStatement>")

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

    write("</letStatement>")
  }

  // Compiles a do statement.
  // Grammar:
  // 'do' subroutineCall ';'
  def compileDo(): Unit = {

    write("<doStatement>")

    // 'do'
    printToken()
    advance()

    // subroutine call
    compileSubroutineCall()

    // ';'
    printToken()
    advance()

    write("</doStatement>")
  }

  // Compiles a subroutine call:
  // functionName() | className.methodName() | varName.methodName()
  def compileSubroutineCall(): Unit = {

    // first identifier
    printToken()
    advance()

    // optional ".method"
    if (token match {
      case Symbol(".") => true
      case _ => false
    }) {

      printToken() // '.'
      advance()

      printToken() // methodName
      advance()
    }

    // '('
    printToken()
    advance()

    // expressionList (TEMPORAIRE vide pour l'instant)
    write("<expressionList></expressionList>")

    // ')'
    printToken()
    advance()
  }

  // Compiles a return statement.
  // Grammar:
  // 'return' expression? ';'
  def compileReturn(): Unit = {

    write("<returnStatement>")

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

    write("</returnStatement>")
  }

  // Compiles a while statement.
  // Grammar:
  // 'while' '(' expression ')' '{' statements '}'
  def compileWhile(): Unit = {

    write("<whileStatement>")

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
    write("<statements>")
    compileStatements()
    write("</statements>")

    // '}'
    printToken()
    advance()

    write("</whileStatement>")
  }

  // Compiles an if statement.
  // Grammar:
  // 'if' '(' expression ')' '{' statements '}' ('else' '{' statements '}')?
  def compileIf(): Unit = {

    write("<ifStatement>")

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
    write("<statements>")
    compileStatements()
    write("</statements>")

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
      write("<statements>")
      compileStatements()
      write("</statements>")

      // '}'
      printToken()
      advance()
    }

    write("</ifStatement>")
  }

  // Compiles an expression.
  // Grammar:
  // term (op term)*
  def compileExpression(): Unit = {

    write("<expression>")

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

    write("</expression>")
  }

  // Compiles a term.
  // A term is a basic unit of an expression.
  def compileTerm(): Unit = {

    write("<term>")

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

      // identifier (varName, array, or subroutine call)
      case Identifier(_) =>
        printToken()
        advance()

        // array access: var[expression]
        if (token match {
          case Symbol("[") => true
          case _ => false
        }) {
          printToken() // '['
          advance()

          compileExpression()

          printToken() // ']'
          advance()
        }

        // subroutine call: . or (
        else if (token match {
          case Symbol("(") | Symbol(".") => true
          case _ => false
        }) {

          compileSubroutineCallAfterIdentifier()
        }

      case _ =>
        throw new RuntimeException("Invalid term")
    }

    write("</term>")
  }

  // Handles subroutine calls that start after an identifier.
  // Grammar:
  // varName.methodName() OR className.methodName()
  def compileSubroutineCallAfterIdentifier(): Unit = {

    // '.' method call
    if (token match {
      case Symbol(".") => true
      case _ => false
    }) {

      printToken() // '.'
      advance()

      printToken() // methodName
      advance()
    }

    // '('
    printToken()
    advance()

    // expressionList (temporary simple version)
    write("<expressionList></expressionList>")

    // ')'
    printToken()
    advance()
  }

  // Compiles a list of expressions.
  // Grammar:
  // (expression (',' expression)*)?
  def compileExpressionList(): Unit = {

    write("<expressionList>")

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

    write("</expressionList>")
  }
}