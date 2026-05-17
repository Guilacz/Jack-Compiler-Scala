import scala.io.Source

class JackTokenizer(input: String) {

  // =========================
  // TYPES OF TOKENS
  // =========================
  sealed trait Token
  case class Keyword(value: String) extends Token
  case class Symbol(value: String) extends Token
  case class Identifier(value: String) extends Token
  case class IntConst(value: Int) extends Token
  case class StringConst(value: String) extends Token

  // =========================
  // VOCABULARY
  // =========================
  val keywords = Set(
    "class","constructor","function","method","field","static",
    "var","int","char","boolean","void","true","false","null","this",
    "let","do","if","else","while","return"
  )

  val symbols = Set(
    "{","}","(",")","[","]",".",",",";","+","-","*","/","&","|","<",">","=","~"
  )

  // =========================
  // CLEAN INPUT
  // =========================
  def clean(code: String): String = {
    code
      .replaceAll("//.*", "")
      .replaceAll("/\\*.*?\\*/", "")
  }

  // =========================
  // TOKENIZE
  // =========================
  def tokenize(): List[Token] = {

    val code = clean(input)
    val chars = code.toList

    var i = 0
    var tokens = List.empty[Token]

    def peek = if (i < chars.length) Some(chars(i)) else None

    def advance() = {
      val c = chars(i); i += 1; c
    }

    def skipWhitespace(): Unit =
      while (peek.exists(_.isWhitespace)) i += 1

    while (i < chars.length) {

      skipWhitespace()

      peek match {

        case Some('"') =>
          advance()
          val sb = new StringBuilder
          while (peek.exists(_ != '"')) sb.append(advance())
          advance()
          tokens ::= StringConst(sb.toString)

        case Some(c) if symbols.contains(c.toString) =>
          tokens ::= Symbol(advance().toString)

        case Some(c) if c.isDigit =>
          val sb = new StringBuilder
          while (peek.exists(_.isDigit)) sb.append(advance())
          tokens ::= IntConst(sb.toString.toInt)

        case Some(c) if c.isLetter || c == '_' =>
          val sb = new StringBuilder
          while (peek.exists(ch => ch.isLetterOrDigit || ch == '_'))
            sb.append(advance())

          val word = sb.toString
          if (keywords.contains(word)) tokens ::= Keyword(word)
          else tokens ::= Identifier(word)

        case Some(_) =>
          advance()

        case None =>
      }
    }

    tokens.reverse
  }

  // =========================
  // XML OUTPUT
  // =========================
  def toXML(tokens: List[Token]): String = {
    def escape(s: String): String = s match {
      case "<" => "&lt;"
      case ">" => "&gt;"
      case "&" => "&amp;"
      case "\"" => "&quot;"
      case other => other
    }

    val body = tokens.map {
      case Keyword(v) => s"<keyword> $v </keyword>"
      case Symbol(v) => s"<symbol> ${escape(v)} </symbol>"
      case Identifier(v) => s"<identifier> $v </identifier>"
      case IntConst(v) => s"<integerConstant> $v </integerConstant>"
      case StringConst(v) => s"<stringConstant> $v </stringConstant>"
    }.mkString("\n")

    "<tokens>\n" + body + "\n</tokens>"
  }
}