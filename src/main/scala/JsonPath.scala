package json

package path {

  sealed abstract class Step(val text: String)
  case class ArrayIndex(index: Int) extends Step("[%s]".format(index))
  case class PropertyName(name: String) extends Step("%s".format(name))

  sealed case class JsonPath(steps: List[Step]) {
    override def toString = steps match {
      case Nil   => "null"
      case steps => steps.reverse.zipWithIndex.map {
        case (PropertyName(name), index) if (index == 0) => name
        case (PropertyName(name), _) => ".%s".format(name)
        case (step, _) => step.text
      }.mkString("")
    }

    def ::(step: Step): JsonPath = this.copy(steps = step :: this.steps)
  }

  object JsonPath {

    def apply(steps: Step*): JsonPath = JsonPath(List(steps: _*))

    def parse(pathText: String): Either[String, JsonPath] = {
      println("PARSE EX: %s".format(JsonPath(toSegments(pathText).flatMap(parseSegment)).steps))
      Right(JsonPath(toSegments(pathText).flatMap(parseSegment).reverse))
    }

    private def toSegments(pathText: String): List[String] = {
      println("SEGMENTS: %s".format(pathText.split('.').toList))
      pathText.split('.').toList
    }

    private def parseSegment(segment: String): List[Step] = {
      println("PARSE SEG: %s".format(segment))
      val index = segment.indexOf('[')
      if (index == -1) {
        List(PropertyName(segment))
      } else if (index == 0) {
        parseArrayIndexes(segment)
      } else {
        PropertyName(segment.substring(0, index)) :: parseArrayIndexes(
          segment.substring(index)
        )
      }
    }

    private def parseArrayIndexes(arrayText: String): List[Step] = {
      arrayText
        .split(']')
        .toList
        .map { x =>
          ArrayIndex(x.substring(1).toInt)
        }
    }
  }

  package pattern {

    sealed abstract class Step(val text: String)
    case class Property(name: String) extends Step(name)
    case object PropertyWildCard extends Step("*")
    case class Index(n: Int) extends Step("[%s]".format(n))
    case object IndexWildCard extends Step("[]")

    sealed case class Pattern(patterns: List[Step]) {
      def matches(jsonPath: JsonPath): Boolean = {
        patterns.zip(jsonPath.steps.reverse).forall {
          case (Property(lhs), PropertyName(rhs))  => lhs == rhs
          case (PropertyWildCard, PropertyName(_)) => true
          case (Index(lhs), ArrayIndex(rhs))       => lhs == rhs
          case (IndexWildCard, ArrayIndex(_))      => true
          case _                                   => false
        }
      }
    }

    object Pattern {
      def apply(steps: Step*): Pattern = Pattern(List(steps: _*))

      def parse(patternText: String): Either[String, Pattern] = {
        Right(Pattern(toSegments(patternText).flatMap(parseSegment)))
      }

      private def toSegments(patternText: String): List[String] =
        patternText.split('.').toList

      private def parseProperty(propertyText: String): List[Step] =
        List(if (propertyText == "*") {
          PropertyWildCard
        } else {
          Property(propertyText)
        })

      private def parseArrayPatterns(arrayText: String): List[Step] =
        arrayText
          .split(']')
          .toList
          .map { x =>
            if (x == "[") {
              IndexWildCard
            } else {
              Index(x.substring(1).toInt)
            }
          }

      private def parseSegment(segment: String): List[Step] = {
        val index = segment.indexOf('[')
        if (index == -1) {
          parseProperty(segment)
        } else if (index == 0) {
          parseArrayPatterns(segment)
        } else {
          parseProperty(segment.substring(0, index)) ::: parseArrayPatterns(
            segment.substring(index)
          )
        }
      }
    }
  }
}
