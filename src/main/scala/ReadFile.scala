import java.io.File
import java.io.PrintWriter
import scala.io.Source

@main def runTranslator(): Unit =
  println("Enter directory path:")
  val dirPath = scala.io.StdIn.readLine()

  val directory = File(dirPath)

  if !directory.exists() || !directory.isDirectory then
    println("Invalid directory")
    return

  val folderName = directory.getName
  val outputFile = File(directory, s"$folderName.asm")

  val writer = PrintWriter(outputFile)

  val vmFiles = directory.listFiles().filter(f => f.isFile && f.getName.endsWith(".vm"))

  var totalBuy = 0.0
  var totalCell = 0.0
  for file <- vmFiles do

    val fileNameWithoutExt = file.getName.stripSuffix(".vm")

    writer.println(fileNameWithoutExt)

    val source = Source.fromFile(file)

    for line <- source.getLines() do

      val cleaned = line.trim

      if cleaned.startsWith("buy") then
        val parts = cleaned.split("\\s+")
        if parts.length >= 4 then
          functions.HandleBuy(parts(1), parts(2).toInt, parts(3).toDouble, writer)
          totalBuy += parts(2).toInt * parts(3).toDouble

      else if cleaned.startsWith("sell") then
        val parts = cleaned.split("\\s+")
        if parts.length >= 4 then
          functions.HandleCell(parts(1), parts(2).toInt, parts(3).toDouble, writer)
          totalCell += parts(2).toInt * parts(3).toDouble


    source.close()

  writer.println("Total Buy: " + totalBuy)
  writer.println("Total Sell: " + totalCell)
  println("Total Buy: " + totalBuy)
  println("Total Sell: " + totalCell)

  writer.close()
  println("Done.")
  writer.close()
  println("Done.")