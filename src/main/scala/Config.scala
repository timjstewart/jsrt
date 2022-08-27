package config

object Config {
  def load(configText: String): Either[String, Config] =
    Right(
      new Config(
        configText.split('\n').foldLeft(Map.empty[String, String])
          { (accum, line) =>
            if (line.trim.startsWith("#")) accum
            else {
              val parts = line.split('=')
              if (parts.length != 2) {
                println("ignoring line: %s".format(line))
                accum
              } else {
                accum + (parts(0).trim -> parts(1).trim)
              }
            }
          }
      ))
}

class Config(values: Map[String, String] = Map.empty) {
  def getKeys(): Set[String] = values.keySet
  def getValue(key: String): Option[String] = values.get(key)
}
