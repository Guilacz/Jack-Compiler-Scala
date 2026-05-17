import scala.io.Source
import java.io.PrintWriter

object Main {

  def main(args: Array[String]): Unit = {

    val input = Source.fromFile(args(0)).mkString

    val tokenizer = new JackTokenizer(input)
    val tokens = tokenizer.tokenize()

    val xml = tokenizer.toXML(tokens)

    val out = new PrintWriter(args(0).replace(".jack", "T.xml"))
    out.write(xml)
    out.close()
  }
}