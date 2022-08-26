import scala.io.Source
import json.path.pattern.Pattern
import json.path.pattern.Property

object Main {
  def main(args: Array[String]): Unit =
    args.foreach { arg =>
      extractJavaScript(arg)
    }

   private def extractJavaScript(file: String): Unit = {
      val text = Source.fromFile(file).getLines().mkString("\n")

      var patterns = Map.empty[Pattern, String]
      Pattern.parse("**.name").foreach {
        pattern => patterns = patterns + (pattern -> "name")
      }

      JsonToJavascriptExtractor.extract(text, patterns) match {
        case Right(javaScript) => writeToFile(javaScript, file + ".js")
        case Left(error)       => Left(error)
      }
    }

  private def writeToFile(text: String, fileName: String): Unit = {
    import java.io.File
    import java.io.PrintWriter
    val writer = new PrintWriter(new File(fileName))
    try writer.write(text)
    finally writer.close()
  }
}

