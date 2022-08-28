import json._
import json.path._
import json.path.pattern._
import parser._

import scala.util.matching.Regex.MatchData
import text.Text._

object JavaScriptToJsonConverter {

  type JsonText = String
  type JavaScriptText = String
  type PathString = String

  type CodeMap = Map[JsonPath, JavaScriptText]

  def merge(
      javaScript: JavaScriptText,
      jsonText: JsonText
  ): Either[String, JsonText] = {
    for {
      jValue <- Parser.parse(jsonText)
      extractCodeMap <- JavaScriptParser.parse(javaScript)
      merged <- injectCodeMap(jValue, extractCodeMap)
    } yield merged
  }

  private object JavaScriptParser {

    // locates the JSON path where the code was extracted from
    val pathRegEx = """/\*\* path\((.*)\) \*/""".r

    // locates the beginning of the generated function that wraps the code
    val functionPrefixRegEx = """function .*(.*) *{\n""".r

    // locates the end of the generated function that wraps the code
    val functionSuffixRegEx = """\n}""".r

    def parse(javaScript: JavaScriptText): Either[String, CodeMap] = {
      for {
        block <- extractFunctions(javaScript)
        extractCodeMap <- combineCodeMaps(block)
      } yield extractCodeMap
    }

    private def extractFunctions(
        javaScript: JavaScriptText
    ): Either[String, List[JavaScriptText]] = {
      def loop(matches: List[MatchData]): List[JavaScriptText] = matches match {
        case first :: second :: rest => // create block between first and second
          javaScript.substring(first.start, second.start) :: loop(
            second :: rest
          )
        case last :: Nil => // create block between first and last
          List(javaScript.substring(last.start))
        case Nil => Nil
      }
      Right(loop(pathRegEx.findAllMatchIn(javaScript).toList))
    }

    private def combineCodeMaps(
        codeBlocks: List[JavaScriptText]
    ): Either[String, CodeMap] = {
      try {
        val codeMaps: List[CodeMap] = for {
          block <- codeBlocks
          Right(body) = extractCodeBody(block)
          Right(map) = extractCodeMap(block, body)
        } yield (map)
        Right(codeMaps.reduce { (lhs, rhs) => lhs ++ rhs })
      } catch {
        case ex: MatchError => Left(ex.getMessage())
      }
    }

    private def extractCodeBody(
        block: JavaScriptText
    ): Either[String, JavaScriptText] = {
      (for {
        prefix <- functionPrefixRegEx.findFirstMatchIn(block)
        suffix <- functionSuffixRegEx.findFirstMatchIn(block)
      } yield block.substring(prefix.end, suffix.start)) match {
        case Some(body) =>
          Right(body.unindent())
        case None => Left("could not get function body from: %s".format(block))
      }
    }

    private def extractCodeMap(
        block: JavaScriptText,
        body: JavaScriptText
    ): Either[String, CodeMap] = {
      pathRegEx.findFirstMatchIn((block)) match {
        case Some(md) =>
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

  private def injectCodeMap(
      jValue: JValue,
      extractCodeMap: CodeMap
  ): Either[String, JsonText] = {
    Right(traverseJson(JsonPath(), jValue, extractCodeMap).prettyPrint)
  }

  private def traverseJson(
      path: JsonPath,
      jValue: JValue,
      extractCodeMap: CodeMap
  ): JValue = {
    jValue match {
      case JArray(elements) =>
        val result = JArray(elements.zipWithIndex.map { case (element, index) =>
          traverseJson(
            ArrayIndex(index) :: path,
            element,
            extractCodeMap
          )
        })
        result

      case JObject(properties) =>
        var newProperties = List.empty[Tuple2[String, JValue]]
        properties.foreach { case (name, value) =>
          newProperties :+= name -> traverseJson(
            PropertyName(name) :: path,
            value,
            extractCodeMap
          )
        }
        JObject(newProperties)

      case JString(value) =>
        JString(extractCodeMap.find { _._1 == path }.map(_._2).getOrElse(value))

      case x =>
        println("Doing nothing for: %s".format(x))
        x
    }
  }
}
