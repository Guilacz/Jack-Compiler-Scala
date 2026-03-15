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

  for file <- vmFiles do

    val fileNameWithoutExt = file.getName.stripSuffix(".vm")

    writer.println(fileNameWithoutExt)

    val source = Source.fromFile(file)

    for line <- source.getLines() do

      val cleaned = line.trim

      if cleaned.startsWith("buy") then
        val parts = cleaned.split("\\s+")
        if parts.length >= 4 then
          //HandleBuy(parts(1), parts(2), parts(3), writer)

      else if cleaned.startsWith("sell") then
        val parts = cleaned.split("\\s+")
        if parts.length >= 4 then
          //HandleSell(parts(1), parts(2), parts(3), writer)

    source.close()

  writer.close()
  println("Done.")