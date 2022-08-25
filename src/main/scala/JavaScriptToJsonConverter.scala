import json._
import json.path._
import json.path.pattern._
import parser._

object JavaScriptToJsonConverter {

  type JsonText = String
  type JavaScriptText = String
  type PathString = String

  type CodeMap = Map[JsonPath, JavaScriptText]

  object JavaScriptParser {

    val pathRegEx = """/\*\* path\((.*)\) \*/""".r
    val functionPrefixRegEx = """function .*(.*) *{\n""".r
    val functionSuffixRegEx = """\n}""".r

    def parse(javaScript: JavaScriptText): Either[String, CodeMap] = {
      for {
        block <- chunkCode(javaScript)
        codeMap <- buildCodeMap(block)
      } yield codeMap
    }

    def chunkCode(
        javaScript: JavaScriptText
    ): Either[String, List[JavaScriptText]] = {
      Right(
        pathRegEx
          .findAllMatchIn(javaScript)
          .toList
          .sliding(2)
          .map {
            case lhs :: rhs :: Nil =>
              javaScript.substring(lhs.start, rhs.start)
            case only :: Nil =>
              javaScript.substring(only.start)
            case x =>
              "MATCHED WRONG!"
          }
          .toList
      )
    }

    def buildCodeMap(
        codeBlocks: List[JavaScriptText]
    ): Either[String, CodeMap] = {
      try {
        val codeMaps: List[CodeMap] = for {
          block <- codeBlocks
          Right(body) = codeBody(block)
          Right(map) = codeMap(block, body)
        } yield (map)
        Right(codeMaps.reduce { (lhs, rhs) => lhs ++ rhs })
      } catch {
        case ex: MatchError => Left(ex.getMessage())
      }
    }

    def codeBody(block: JavaScriptText): Either[String, JavaScriptText] = {
      (for {
        prefix <- functionPrefixRegEx.findFirstMatchIn(block)
        suffix <- functionSuffixRegEx.findFirstMatchIn(block)
      } yield block.substring(prefix.end, suffix.start)) match {
        case Some(body) =>
          Right(unindent(body))
        case None => Left("could not get function body from: %s".format(block))
      }
    }

    def codeMap(block: JavaScriptText, body: JavaScriptText): Either[String, CodeMap] = {
      pathRegEx.findFirstMatchIn((block)) match {
        case         Some(md) =>
          JsonPath.parse(md.group(1)) match {
            case Right(path) =>
              Right(Map(path -> body))
            case Left(error) => Left(error)
          }
        case None => Left("no code map found in block: %s".format(block))
      }
    }

    private def unindent(text: String): String = {
      val canIndent = text.split('\n').forall(p => p.startsWith("  "))
      if (canIndent) {
        text.split('\n').map(s => s.substring(2)).mkString("\n")
      } else {
        text
      }
    }

    private def removeFunctionWrapper(
        javaScript: JavaScriptText
    ): Either[String, JavaScriptText] = {
      (for {
        prefixMatch <- functionPrefixRegEx.findFirstMatchIn(javaScript)
        suffixMatch <- functionSuffixRegEx
          .findAllMatchIn(javaScript)
          .toList
          .reverse
          .headOption
      } yield javaScript.substring(prefixMatch.end, suffixMatch.start))
        .toRight(
          "could not remove function wrapper from: %s".format(javaScript)
        )
    }
  }

  def merge(
      javaScript: JavaScriptText,
      jsonText: JsonText
  ): Either[String, JsonText] = {
    for {
      jValue <- Parser.parse(jsonText)
      codeMap <- JavaScriptParser.parse(javaScript)
      merged <- injectCodeMap(jValue, codeMap)
    } yield merged
  }

  private def injectCodeMap(
      jValue: JValue,
      codeMap: CodeMap
  ): Either[String, JsonText] = {
    Right(traverseJson(JsonPath(), jValue, codeMap).prettyPrint)
  }

  private def traverseJson(
      path: JsonPath,
      jValue: JValue,
      codeMap: CodeMap,
  ): JValue = {
    jValue match {
      case JArray(elements) =>
        val result = JArray(elements.zipWithIndex.map { case (element, index) =>
          traverseJson(
            ArrayIndex(index) :: path,
            element,
            codeMap
          )
        })
        result

      case JObject(properties) =>
        var newProperties = List.empty[Tuple2[String, JValue]]
        properties.foreach { case (name, value) =>
          newProperties :+= name -> traverseJson(
            PropertyName(name) :: path,
            value,
            codeMap
          )
        }
        JObject(newProperties)

      case JString(value) =>
        JString(codeMap.find {_._1 == path}.map(_._2).getOrElse(value))

      case x =>
        println("Doing nothing for: %s".format(x))
        x
    }
  }
}
