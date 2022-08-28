import scala.io.Source

import json.path.pattern.Pattern
import json.path.pattern.Property
import config.Config

object Main {

  def main(args: Array[String]): Unit = {
    val config = loadConfig()
    args.foreach { arg =>
      if (arg.endsWith(".json")) {
        println("extracting JavaScript from: %s".format(arg))
        extractJavaScript(config, arg)
      } else if (arg.endsWith(".js")) {
        println("merging JavaScript from: %s".format(arg))
        mergeJavaScriptIntoJson(config, arg)
      } else {
        println("ignoring file: %s".format(arg))
      }
    }
  }

  private def extractJavaScript(config: Config, file: String): Unit = {
    try {
      val text = Source.fromFile(file).getLines().mkString("\n")

      var patterns = Map.empty[Pattern, String]
      Pattern.parse(config.getValue("functionNamePattern", "**.name")).foreach {
        pattern =>
          patterns = patterns + (pattern -> "name")
      }

      JsonToJavascriptExtractor.extract(text, patterns) match {
        case Right(javaScript) => writeToFile(javaScript, file + ".js")
        case Left(error)       => println("error: %s".format(error))
      }
    } catch {
      case ex: java.io.FileNotFoundException =>
        println("error: %s".format(ex.getMessage()))
    }
  }

  private def mergeJavaScriptIntoJson(config: Config, file: String): Unit = {
    try {
      val jsonFile = file.replace(".json.js", ".json")
      val outputFile = file.replace(".json.js", ".out.json")

      val javaScript = Source.fromFile(file).getLines().mkString("\n")
      val json = Source.fromFile(jsonFile).getLines().mkString("\n")

      JavaScriptToJsonConverter.merge(javaScript, json) match {
        case Right(jsonText) => writeToFile(jsonText, outputFile)
        case Left(error)     => println("error: %s".format(error))
      }
    } catch {
      case ex: java.io.FileNotFoundException =>
        println("error: %s".format(ex.getMessage()))
    }
  }

  private def writeToFile(text: String, fileName: String): Unit = {
    import java.io.File
    import java.io.PrintWriter
    val writer = new PrintWriter(new File(fileName))
    try writer.write(text)
    finally writer.close()
  }

  private def loadConfig(): Config = {
    Option(System.getProperty("user.home"))
      .flatMap { userHome =>
        Config.loadFromFile(userHome + "/.config/jsrt/jsrt.conf").toOption
      }
      .getOrElse(Config.empty)
  }
}
