package json.path.pattern

import json.path._

sealed abstract class Step(val text: String)
case class Property(name: String) extends Step(name)
case object PropertyWildCard extends Step("*")
case class Index(n: Int) extends Step("[%s]".format(n))
case object IndexWildCard extends Step("[]")
case object DeepWildCard extends Step("**")

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
    } else if (propertyText == "**") {
      DeepWildCard
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
