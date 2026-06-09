// CompilationEngine.scala
// : generates  VM code from jack tokens

import java.io.PrintWriter

class CompilationEngine(tokens: List[Token], output: PrintWriter) {

  val vm    = new VMWriter(output)
  val table = new SymbolTable

  var i = 0
  def token: Token = tokens(i)
  def advance(): Unit = i += 1
  def peekNext: Option[Token] = tokens.lift(i + 1)

  var className = ""

  // counter for labels
  var labelCounter = 0
  def newLabel(prefix: String): String = {
    val lbl = s"${prefix}_$labelCounter"
    labelCounter += 1
    lbl
  }

  // -------------------------------------------------------
  // HELPERS to eat tokens without writing them
  // -------------------------------------------------------

  //returns string value of current token and goes on
  def eat(): String = token match {
    case Keyword(v)    => advance(); v
    case Symbol(v)     => advance(); v
    case Identifier(v) => advance(); v
    case IntConst(v)   => advance(); v.toString
    case StringConst(v)=> advance(); v
  }

  def eatKeyword(): String  = { val Keyword(v)    = token; advance(); v }
  def eatSymbol(): String   = { val Symbol(v)     = token; advance(); v }
  def eatIdentifier(): String = { val Identifier(v) = token; advance(); v }
  def eatInt(): Int         = { val IntConst(v)   = token; advance(); v }
  def eatString(): String   = { val StringConst(v)= token; advance(); v }

  def isSymbol(s: String): Boolean = token match {
    case Symbol(v) => v == s
    case _ => false
  }

  def isKeyword(k: String): Boolean = token match {
    case Keyword(v) => v == k
    case _ => false
  }

  // -------------------------------------------------------
  // COMPILE CLASS
  // -------------------------------------------------------
  def compileClass(): Unit = {
    eatKeyword() // 'class'
    className = eatIdentifier() // className
    eatSymbol() // '{'

    // class variable declarations
    while (isKeyword("static") || isKeyword("field")) {
      compileClassVarDec()
    }

    // subroutine declarations
    while (isKeyword("constructor") || isKeyword("function") || isKeyword("method")) {
      compileSubroutine()
    }

    eatSymbol() // '}'
  }

  // -------------------------------------------------------
  // COMPILE CLASS VAR DEC
  // -------------------------------------------------------
  def compileClassVarDec(): Unit = {
    val kind = eatKeyword() // 'static' | 'field'
    val tpe = eat() // type
    val name = eatIdentifier() // varName
    table.define(name, tpe, kind)

    while (isSymbol(",")) {
      eatSymbol()
      val n = eatIdentifier()
      table.define(n, tpe, kind)
    }

    eatSymbol() // ';'
  }

  // -------------------------------------------------------
  // COMPILE SUBROUTINE
  // -------------------------------------------------------
  def compileSubroutine(): Unit = {
    table.startSubroutine()

    val subroutineKind = eatKeyword() // 'constructor' | 'function' | 'method'
    eat() // return type
    val subroutineName = eatIdentifier()
    val fullName = s"$className.$subroutineName"

    // For a method, 'this' is the implicit argument 0
    if (subroutineKind == "method") {
      table.define("this", className, "arg")
    }

    eatSymbol() // '('
    compileParameterList()
    eatSymbol() // ')'

    // Body
    eatSymbol() // '{'

    // Count the variable declarations to know how many premises
    while (isKeyword("var")) compileVarDec()

    val nLocals = table.varCount("var")
    vm.writeFunction(fullName, nLocals)

    // Prologue depending on the type of subroutine
    subroutineKind match {
      case "constructor" =>
        val nFields = table.varCount("field")
        vm.writePush("constant", nFields)
        vm.writeCall("Memory.alloc", 1)
        vm.writePop("pointer", 0) // anchor THIS to the allocated object

      case "method" =>
        vm.writePush("argument", 0)
        vm.writePop("pointer", 0) // anchors THIS to the calling object

      case _ => // function : nothing
    }

    compileStatements()
    eatSymbol() // '}'
  }
}