import scala.io.Source

object Main {
  def main(args: Array[String]): Unit =
    args.foreach { arg =>
      val text = Source.fromFile(arg).getLines().mkString("\n")
      JsonToJavascriptExtractor.extract(text) match {
        case Right(javaScript) => write(javaScript, arg + ".js")
        case Left(error)       => Left(error)
      }
    }

  private def write(text: String, fileName: String): Unit = {
    import java.io.File
    import java.io.PrintWriter
    val writer = new PrintWriter(new File(fileName))
    try writer.write(text)
    finally writer.close()
  }
}
