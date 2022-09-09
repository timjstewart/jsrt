import scala.io.Source
import java.io.File
import java.io.PrintWriter

import json.path.pattern.Pattern
import json.path.pattern.Property
import config.Config
import arguments.Arguments

object Main {

  def main(args: Array[String]): Unit = {
    Arguments.parse(args).map { arguments =>
      val config = loadConfig(arguments.configFile)
      arguments.files.foreach { fileName =>
        if (fileName.endsWith(".json")) {
          println("extracting JavaScript from: %s".format(fileName))
          extractJavaScript(config, fileName)
        } else if (fileName.endsWith(".js")) {
          println("merging JavaScript from: %s".format(fileName))
          mergeJavaScriptIntoJson(config, fileName)
        } else {
          println("ignoring file: %s".format(fileName))
        }
      }
    }
  }

  private def extractJavaScript(config: Config, file: String): Unit = {
    try {
      val text = Source.fromFile(file).getLines().mkString("\n")
      (for {
        functionNamePattern <- config.getPattern("functionNamePattern")
        exportPattern <- config.getPattern("exportPattern")
      } yield {
        val patterns = Map(functionNamePattern -> "functionName")
        val exportPatterns = List(exportPattern)
        JsonToJavascriptExtractor.extract(
          text,
          exportPatterns,
          patterns
        ) match {
          case Right(javaScript) => writeToFile(javaScript, file + ".js")
          case Left(error)       => println("error: %s".format(error))
        }
      }) match {
        case Left(error) => println("error: %s".format(error))
        case Right(_)    => ()
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
    val writer = new PrintWriter(new File(fileName))
    try writer.write(text)
    finally writer.close()
  }

  private def loadConfig(configFile: Option[String]): Config = {
    replaceTilde(configFile.getOrElse( "~/.config/jsrt/jsrt.conf"))
      .flatMap(replaceTilde)
      .flatMap {
        fileName => Config.loadFromFile(fileName).toOption
      }
      .getOrElse(Config.empty)
  }

  private def replaceTilde(fileName: String): Option[String] = {
    if (fileName.startsWith("~")) {
      Option(System.getProperty("user.home")).map { userHome =>
        fileName.replaceFirst("~", userHome)
      }
    } else {
      Some(fileName)
    }
  }
}
