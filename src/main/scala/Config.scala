package config

import json.path.pattern.Pattern

import scala.io.Source

object Config {

  val empty: Config = new Config(Map.empty[String, String])

  def loadFromFile(fileName: String): Either[String, Config] = {
    try {
      loadFromString(Source.fromFile(fileName).getLines().mkString("\n"))
    } catch {
      case ex: java.io.FileNotFoundException => {
        Right(Config.empty)
      }
    }
  }

  def loadFromString(configText: String): Either[String, Config] =
    Right(
      new Config(
        configText.split('\n').foldLeft(Map.empty[String, String]) {
          (accum, line) =>
            if (line.trim.startsWith("#")) { accum }
            else {
              val parts = line.split('=')
              if (parts.length != 2) {
                accum
              } else {
                accum + (parts(0).trim -> parts(1).trim)
              }
            }
        }
      )
    )
}

class Config(values: Map[String, String] = Map.empty) {
  def getKeys(): Set[String] = values.keySet
  def getValue(key: String): Option[String] = values.get(key)
  def getPattern(key: String): Either[String, Pattern] = values.get(key) match {
    case None => Left("no configuration value for key: %s".format(key))
    case Some(value) => Pattern.parse(value)
  }
  def getValue(key: String, default: String): String =
    values.getOrElse(key, default)
}
