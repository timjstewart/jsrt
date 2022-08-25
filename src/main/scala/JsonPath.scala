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
      Right(JsonPath(toSegments(pathText).flatMap(parseSegment).reverse))
    }

    private def toSegments(pathText: String): List[String] = {
      pathText.split('.').toList
    }

    private def parseSegment(segment: String): List[Step] = {
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
}
