package json.path.pattern

import json.path._
import json.path.{Step => PathStep}

sealed abstract class Step(val text: String)
case class Property(name: String) extends Step(name)
case object PropertyWildCard extends Step("*")
case class Index(n: Int) extends Step("[%s]".format(n))
case object IndexWildCard extends Step("[]")
case object DeepWildCard extends Step("**")

sealed case class Pattern(patterns: List[Step]) {
  def matches(jsonPath: JsonPath): Boolean = {

    @annotation.tailrec
    def loop(patternSteps: List[Step], pathSteps: List[PathStep]): Boolean = {
      patternSteps match {

        case Property(lhs) :: lhsRest =>
          pathSteps match {
            case PropertyName(rhs) :: rhsRest =>
              lhs == rhs && loop(lhsRest, rhsRest)
            case _ => false
          }

        case PropertyWildCard :: lhsRest =>
          pathSteps match {
            case PropertyName(name) :: rhsRest => loop(lhsRest, rhsRest)
            case _                             => false
          }

        case Index(lhs) :: lhsRest =>
          pathSteps match {
            case ArrayIndex(rhs) :: rhsRest =>
              lhs == rhs && loop(lhsRest, rhsRest)
            case _ => false
          }

        case IndexWildCard :: lhsRest =>
          pathSteps match {
            case ArrayIndex(_) :: rhsRest => loop(lhsRest, rhsRest)
            case _                        => false
          }

        // If the lookAhead pattern step matches the next path step, then
        // consume the lookAhead pattern step and the lookAhead, as well as the
        // next pattern step.
        case DeepWildCard :: lookAhead :: lhsRest =>
          lookAhead match {
            case Property(lhs) =>
              pathSteps match {
                case Nil                      => true
                case ArrayIndex(_) :: rhsRest => loop(patternSteps, rhsRest)
                case PropertyName(rhs) :: rhsRest =>
                  if (lhs == rhs) { loop(lhsRest, rhsRest) }
                  else { loop(patternSteps, rhsRest) }
              }

            case PropertyWildCard =>
              pathSteps match {
                case Nil                      => true
                case ArrayIndex(_) :: rhsRest => loop(patternSteps, rhsRest)
                case PropertyName(_) :: rhsRest =>
                  loop(lookAhead :: lhsRest, rhsRest)
              }

            case Index(lhs) =>
              pathSteps match {
                case Nil => true
                case ArrayIndex(rhs) :: rhsRest =>
                  lhs == rhs && loop(lhsRest, rhsRest)
                case PropertyName(name) :: rhsRest =>
                  loop(patternSteps, rhsRest)
              }

            case IndexWildCard =>
              pathSteps match {
                case Nil                        => true
                case ArrayIndex(_) :: rhsRest   => loop(lhsRest, rhsRest)
                case PropertyName(_) :: rhsRest => loop(patternSteps, rhsRest)
              }

            case DeepWildCard =>
              pathSteps match {
                case Nil                      => true
                case ArrayIndex(_) :: rhsRest => loop(patternSteps, rhsRest)
                case PropertyName(_) :: rhsRest =>
                  loop(lookAhead :: lhsRest, rhsRest)
              }
          }

        case DeepWildCard :: Nil => true

        case Nil =>
          pathSteps match {
            case Nil => true
            case _   => false
          }
      }
    }

    loop(patterns.reverse, jsonPath.steps)
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
