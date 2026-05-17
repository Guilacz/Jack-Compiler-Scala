import scala.io.{Source, StdIn}
import java.io.File
import java.io.PrintWriter

object Main {

  def main(args: Array[String]): Unit = {

    val path =
      if (args.length == 1) args(0)
      else {
        println("Enter path to .jack file:")
        StdIn.readLine()
      }

    val inputFile = new File(path)
    val inputCode = Source.fromFile(inputFile).mkString

    val tokenizer = new JackTokenizer(inputCode)
    val tokens = tokenizer.tokenize()

    val xml = tokenizer.toXML(tokens)

    // =========================================================================
    // LOGIQUE DE TRI AUTOMATIQUE PAR DOSSIER DE PROJET
    // =========================================================================

    // Récupère le nom du dossier contenant le fichier (ex: "ExpressionLessSquare" ou "ArrayTest")
    val projectFolderName = inputFile.getParentFile.getName

    // Structure cible : Outputs / outputs stage 4 / [NomDuProjet]
    val outputDir = new File(s"Outputs/outputs stage 4/$projectFolderName")

    // Crée toute l'arborescence de dossiers si elle n'existe pas
    if (!outputDir.exists()) outputDir.mkdirs()

    // Extraction du nom de fichier (ex: Main.jack -> MainT.xml)
    val fileName = inputFile.getName.replace(".jack", "T.xml")

    // Chemin complet du fichier final
    val outputFile = new File(outputDir, fileName)
    val outPath = outputFile.getPath

    // Écriture du fichier XML
    val out = new PrintWriter(outputFile)
    out.write(xml)
    out.close()

    println("DONE -> " + outPath)
  }
}