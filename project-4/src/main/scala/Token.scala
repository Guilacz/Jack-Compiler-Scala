sealed trait Token

// =========================
// TYPES OF TOKENS
// =========================
case class Keyword(value: String) extends Token
case class Symbol(value: String) extends Token
case class Identifier(value: String) extends Token
case class IntConst(value: Int) extends Token
case class StringConst(value: String) extends Token
