import json.path._
import json.path.pattern._

object JavaScriptToJsonConverter {

  type JsonText = String
  type JavaScriptText = String
  type PathString = String

  type CodeMap = Map[JsonPath, JavaScriptText]

  object JavaScriptParser {

    val pathRegEx = """/\*\* path\((.*)\) \*/""".r
    val functionPrefixRegEx = """function .*(.*) *{""".r
    val functionSuffixRegEx = """}""".r

    def parse(javaScript: JavaScriptText): Either[String, CodeMap] = {
      for {
        block <- chunkCode(javaScript)
        codeMap <- buildCodeMap(block)
      } yield codeMap
    }

    def chunkCode(
        javaScript: JavaScriptText
    ): Either[String, List[JavaScriptText]] = {
      println("Chunk [%s]".format(javaScript))
      Right(
        pathRegEx
          .findAllMatchIn(javaScript)
          .toList
          .sliding(2)
          .map {
            case lhs :: rhs :: Nil =>
              println("2: %s -> %s".format(lhs, rhs))
              javaScript.substring(lhs.start, rhs.start)
            case only :: Nil =>
              println("1: %s".format(only))
              javaScript.substring(only.start)
            case x =>
              println("0: %s".format(x))
              "REST"
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
      println("GETTING BODY FROM: %s".format(block))
      (for {
        prefix <- functionPrefixRegEx.findFirstMatchIn(block)
        suffix <- functionSuffixRegEx.findFirstMatchIn(block)
      } yield block.substring(prefix.end, suffix.start)) match {
        case Some(body) =>
          println("BODY: %s".format(body))
          Right(body)
        case None => Left("could not get function body from: %s".format(block))
      }
    }

    def codeMap(block: JavaScriptText, body: JavaScriptText): Either[String, CodeMap] = {
      println("GETTING CODE MAP FROM: %s".format(block))
      pathRegEx.findFirstMatchIn((block)) match {
        case         Some(md) =>
          JsonPath.parse(md.group(1)) match {
            case Right(path) =>
              println("PATH: %s - %s, BODY: %s".format(path.steps, path, body))
              Right(Map(path -> body))
            case Left(error) => Left(error)
          }
        case None => Left("no code map found in block: %s".format(block))

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
    println("INJECT: %s into %s".format(codeMap, jValue))
    Left("errror")
  }

  // private def traverseJson(
  //     path: JsonPath,
  //     jValue: JValue,
  //     codeMap: CodeMap, JavaScriptText],
  // ): JValue = {
  //   jValue match {
  //     case JArray(elements) =>
  //       elements.zipWithIndex.foreach { case (element, index) =>
  //         traverseJson(
  //           ArrayIndex(index) :: path,
  //           element,
  //           codeMap
  //         )
  //       }
  //     case JObject(properties) =>
  //       properties.foreach { case (name, value) =>
  //         traverseJson(
  //           PropertyName(name) :: path,
  //           value,
  //           codeMap
  //         )
  //       }
  //     case JString(value) =>
  //       codeMap.foreach { case (jsonPath: JsonPath, code: JavaScriptText) =>
  //         if (jsonPath.matches(path)) {
  //           println("HI")
  //         }
  //       }
  //     case _ => ()
  //   }
  //   output
  // }

}
