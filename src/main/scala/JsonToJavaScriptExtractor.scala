object JsonToJavascriptExtractor {

  import json.path._
  import json.path.pattern._

  type SourceCode = String

  def extract(
      jsonText: String,
      patterns: Map[Pattern, String] = Map.empty
  ): Either[String, SourceCode] =
    Parser.parse(jsonText) match {
      case Right(json) =>
        Right(
          traverseJson(
            JsonPath(),
            json,
            new StringBuffer(),
            patterns
          ).toString.trim
        )
      case Left(error) => Left(error)
    }

  private def traverseJson(
      path: JsonPath,
      jValue: JValue,
      output: StringBuffer,
      patterns: Map[Pattern, String],
      collected: scala.collection.mutable.Map[String, String] =
        scala.collection.mutable.Map.empty
  ): StringBuffer = {
    jValue match {
      case JArray(elements) =>
        elements.zipWithIndex.foreach { case (element, index) =>
          traverseJson(
            ArrayIndex(index) :: path,
            element,
            output,
            patterns,
            collected
          )
        }
      case JObject(properties) =>
        properties.foreach { case (name, value) =>
          traverseJson(
            PropertyName(name) :: path,
            value,
            output,
            patterns,
            collected
          )
        }
      case JString(value) =>
        patterns.foreach { case (pattern: Pattern, name: String) =>
          if (pattern.matches(path)) {
            collected += (name -> value)
          }
        }
        if (shouldExport(path)) {
          exportFunction(output, path, value, collected)
        }
      case _ => ()
    }
    output
  }

  private def exportFunction(
      output: StringBuffer,
      path: JsonPath,
      text: String,
      collected: scala.collection.mutable.Map[String, String]
  ): Unit = {
    output.append("""
/** path(%s) */
function %s() {
%s
}
""".format(path, jsFuncName(collected.get("name")), indent(text)))
  }

  private def jsFuncName(name: Option[String]): String = name match {
    case None       => "func"
    case Some(name) => name.replace(' ', '_')
  }

  private def leftJustify(text: String): String = {
    // compute how many spaces the text can be moved to the left
    val numSpaces = text
      .split('\n')
      .flatMap(s =>
        if (s.trim.isEmpty()) { Nil }
        else {
          List(Math.max(0, s.indexWhere(c => !c.isWhitespace)))
        }
      ).min

    // move the text over to the left
    text
      .split('\n')
      .map(x =>
        if (x.length > numSpaces) {
          x.substring(numSpaces)
        } else {
          // TODO: Could this just be a blank line?
          x
        }
      )
      .mkString("\n")
  }

  private def indent(text: String): String = {
    return leftJustify(text)
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
