import scala.io.{Source, StdIn}
import java.io.File
import java.io.PrintWriter

object Main {

  def main(args: Array[String]): Unit = {

    val path =
      if (args.length == 1) {
        args(0)
      } else {
        println("Enter path to .jack file:")
        StdIn.readLine()
      }

    val tokenizer = new JackTokenizer(path)
    val tokens = tokenizer.tokenize()

    val xml = tokenizer.toXML(tokens)

    val out = new PrintWriter(path.replace(".jack", "T.xml"))
    out.write(xml)
    out.close()
  }
}