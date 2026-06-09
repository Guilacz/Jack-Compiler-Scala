
// manages variables of a class and of subroutine
// each variable has : name -> (type, kind, index)

class SymbolTable {

  // Counteurs by kind
  private var counts = Map(
    "static" -> 0,
    "field"  -> 0,
    "arg"    -> 0,
    "var"    -> 0
  )

  // Table class-level (static, field)
  private var classTable = Map.empty[String, (String, String, Int)]

  // Table subroutine-level (arg, var)
  private var subroutineTable = Map.empty[String, (String, String, Int)]

  // Reset  table subroutine (call in the beginning of each  subroutine)
  def startSubroutine(): Unit = {
    subroutineTable = Map.empty
    counts = counts.updated("arg", 0).updated("var", 0)
  }

  // add  variable in the right table
  def define(name: String, tpe: String, kind: String): Unit = {
    val idx = counts(kind)
    counts = counts.updated(kind, idx + 1)
    kind match {
      case "static" | "field" => classTable    = classTable    + (name -> (tpe, kind, idx))
      case "arg"    | "var"   => subroutineTable = subroutineTable + (name -> (tpe, kind, idx))
    }
  }

  def varCount(kind: String): Int = counts(kind)

  private def lookup(name: String): Option[(String, String, Int)] =
    subroutineTable.get(name).orElse(classTable.get(name))

  def kindOf(name: String): Option[String]  = lookup(name).map(_._2)
  def typeOf(name: String): Option[String]  = lookup(name).map(_._1)
  def indexOf(name: String): Option[Int]    = lookup(name).map(_._3)

  // convert the intern kind in segment VM
  def segmentOf(name: String): Option[String] = kindOf(name).map {
    case "static" => "static"
    case "field"  => "this"
    case "arg"    => "argument"
    case "var"    => "local"
  }
}
