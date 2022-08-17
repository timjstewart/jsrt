package json

package path {

  sealed abstract class Step(val text: String)
  case class ArrayIndex(index: Int) extends Step("[%s]".format(index))
  case class PropertyName(name: String) extends Step(".%s".format(name))

  sealed case class JsonPath(steps: List[Step]) {
    override def toString = steps match {
      case Nil   => "null"
      case steps => steps.reverse.map(_.text).mkString("")
    }

    def ::(step: Step): JsonPath = this.copy(steps = step :: this.steps)
  }

  object JsonPath {
    def apply(steps: Step*): JsonPath = JsonPath(List(steps: _*))
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

      // TODO: make this less ugly
      def parse(patternText: String): Either[String, Pattern] = {
        Right(Pattern(patternText.split('.').toList flatMap { segment =>
          val index = segment.indexOf('[')
          if (index == -1) {
            List(if (segment == "*") {
              PropertyWildCard
            } else {
              Property(segment)
            })
          } else {
            val indexPatterns = segment
              .substring(index)
              .split(']')
              .toList
              .map { x =>
                if (x == "[") {
                  IndexWildCard
                } else {
                  Index(x.substring(1).toInt)
                }
              }
            val head = if (segment.substring(0, index) == "*") {
              PropertyWildCard
            } else {
              Property(segment.substring(0, index))
            }
            head :: indexPatterns
          }
        }))
      }
    }
  }

}
