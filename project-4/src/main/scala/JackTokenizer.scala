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