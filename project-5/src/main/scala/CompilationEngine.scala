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
  }}