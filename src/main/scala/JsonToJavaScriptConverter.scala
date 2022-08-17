object JsonToJavascriptExtractor {

  import json.path._

  type SourceCode = String

  def extract(jsonText: String): Either[String, SourceCode] =
    Parser.parse(jsonText) match {
      case Right(json) =>
        Right(traverseJson(JsonPath(), json, new StringBuffer()).toString.trim)
      case Left(error) => Left(error)
    }

  private def traverseJson(
      path: JsonPath,
      jValue: JValue,
      output: StringBuffer
  ): StringBuffer = {
    jValue match {
      case JArray(elements) =>
        elements.zipWithIndex.foreach { case (element, index) =>
          traverseJson(ArrayIndex(index) :: path, element, output)
        }
      case JObject(properties) =>
        properties.foreach { case (name, value) =>
          traverseJson(PropertyName(name) :: path, value, output)
        }
      case JString(value) if shouldExport(path) =>
        output.append("""
/** ref(%s) */
function func() {
%s
}
""".format(path, indent(value)))
      case _ => ()
    }
    output
  }

  private def indent(text: String): String = {
    return text
      .split("\n")
      .filter(_.length() > 0)
      .map(line => "    %s".format(line))
      .mkString("\n")
  }

  private def shouldExport(path: JsonPath): Boolean = path match {
    case JsonPath(PropertyName(name) :: _) if name == "jsCode" => true
    case _                                                     => false
  }
}
