import java.io.PrintWriter

object functions {
  def HandleBuy(productName: String, amount: Int, price: Double, writer: PrintWriter): Unit = {
    writer.println("### BUY " + productName + " ###")
    writer.println(amount + "*" + price)
  }

  def HandleCell(productName: String, amount: Int, price: Double, writer: PrintWriter): Unit = {
    writer.println("### CELL " + productName + " ###")
    writer.println(amount + "*" + price)
  }
}
