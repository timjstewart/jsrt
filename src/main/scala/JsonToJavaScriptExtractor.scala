import parser._
import json._
import json.path._
import json.path.pattern._

object JsonToJavascriptExtractor {

  type SourceCode = String

  /** extracts JavaScript code from a JSON string
    *
    * @param jsonText
    *   the JSON text that contains JavaScript function bodies.
    * @param patterns
    *   a map from patterns to pattern names. As the JSON document is processed,
    *   strings that match the patterns in this map will be remembered and made
    *   available for use when source code is being rendered.
    * @return
    *   either an error message or the extracted JavaScript source code
    */
  def extract(
      jsonText: String,
      exportPatterns: List[Pattern],
      patterns: Map[Pattern, String] = Map.empty
  ): Either[String, SourceCode] =
    Parser.parse(jsonText) match {
      case Right(json) =>
        Right(
          traverseJson(
            JsonPath(),
            json,
            new StringBuffer(),
            patterns,
            exportPatterns
          ).toString.trim
        )
      case Left(error) => Left(error)
    }

  private def traverseJson(
      path: JsonPath,
      jValue: JValue,
      output: StringBuffer,
      patterns: Map[Pattern, String],
      exportPatterns: List[Pattern],
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
            exportPatterns,
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
            exportPatterns,
            collected
          )
        }
      case JString(value) =>
        patterns.foreach { case (pattern: Pattern, name: String) =>
          if (pattern.matches(path)) {
            collected += (name -> value)
          }
        }
        if (shouldExport(exportPatterns, path)) {
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
""".format(path, jsFuncName(collected.get("functionName")), indent(text)))
  }

  private def jsFuncName(name: Option[String]): String = name match {
    case None       => "func"
    case Some(name) => makeLegalIdentifier(name)
  }

  private def makeLegalIdentifier(s: String): String =
    s.replace(' ', '_').replace('-', '_').replace('@', '_')

  private def leftJustify(text: String): String = {
    // compute how many spaces the text can be moved to the left
    val numSpaces = text
      .split('\n')
      .flatMap(s =>
        if (s.trim.isEmpty()) { Nil }
        else {
          List(Math.max(0, s.indexWhere(c => !c.isWhitespace)))
        }
      )
      .min

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
    leftJustify(text)
      .split("\n")
      .filter(_.length() > 0)
      .map(line => "    %s".format(line))
      .mkString("\n")
  }

  private def shouldExport(
      exportPatterns: List[Pattern],
      path: JsonPath
  ): Boolean = path match {
    case JsonPath(PropertyName(name) :: _) =>
      exportPatterns.exists(p => p.matches(path))
    case _ => false
  }
}
