package parser

import json._

object Parser {
  import lexer._
  import lexer.Lexer._

  val JTrue = JBool(true)
  val JFalse = JBool(false)

  type Result = Either[String, JValue]

  def parse(json: String): Result = Lexer.tokenize(json) match {
    case Left(error) => Left(error)
    case Right(Tuple2(tokens, remaining)) =>
      val result = tokens.foldLeft[Either[String, List[JValue]]](
        Right(List.empty[JValue])
      )((stack, token) => foldToken(stack, token))
      result match {
        case Right(JProperty(obj, name) :: Nil) =>
          Left(
            "object property: '%s' never given a value: %s".format(name, obj)
          )
        case Right(result :: Nil) => Right(result)
        case _ => Left("malformed JSON document: %s".format(result))
      }
  }

  private def foldToken(
      stack: Either[String, List[JValue]],
      token: Token
  ) = {
    stack match {
      case Left(error) => Left(error)
      case Right(stack) => {
        token match {
          case OpenSquareBracketToken(_) =>
            push(stack, JArray())
          case OpenCurlyBraceToken(_) =>
            push(stack, JObject())
          case CloseSquareBracketToken(_) =>
            pop(stack) match {
              case Right(Tuple2(popped, newStack)) =>
                jValue(newStack, popped)
              case _ => Left("unexpected ]: %s".format(stack))
            }
          case CloseCurlyBraceToken(_) =>
            pop(stack) match {
              case Right(Tuple2(popped, newStack)) =>
                jValue(newStack, popped)
              case _ => Left("unexpected }: %s".format(stack))
            }
          case ColonToken(_)          => colon(stack)
          case StringToken(value, _)  => string(stack, value)
          case BoolTrueToken(_)       => jValue(stack, JTrue)
          case BoolFalseToken(_)      => jValue(stack, JFalse)
          case NullToken(_)           => jValue(stack, JNull)
          case NumberToken(number, _) => jValue(stack, JNumber(number))
          case EOF(_)                 => Right(stack)
          case CommaToken(_)          => Right(stack)
          case _                      => Right(stack)
        }
      }
    }
  }

  private def push(
      stack: List[JValue],
      jValue: JValue
  ): Either[String, List[JValue]] = jValue match {
    case obj @ JObject(_)  => Right(obj :: stack)
    case array @ JArray(_) => Right(array :: stack)
    case _ => Left("cannot push %s onto stack: %s".format(jValue, stack))
  }

  private def pop(
      stack: List[JValue]
  ): Either[String, Tuple2[JValue, List[JValue]]] =
    stack match {
      case (array @ JArray(_)) :: tail => Right(Tuple2(array, tail))
      case (obj @ JObject(_)) :: tail  => Right(Tuple2(obj, tail))
      case Nil                         => Left("stack underflow")
      case _ => Left("unexpected element popped: %s".format(stack))
    }

  private def colon(stack: List[JValue]): Either[String, List[JValue]] =
    stack match {
      case JProperty(obj, name) :: _ => Right(stack)
      case _ => Left("unexpected colon: %s".format(stack))
    }

  private def string(
      stack: List[JValue],
      string: String
  ): Either[String, List[JValue]] = stack match {
    case JArray(elements) :: tail =>
      Right(JArray(elements :+ JString(string)) :: tail)
    case (obj @ JObject(properties)) :: tail =>
      Right(JProperty(obj, string) :: tail)
    case JProperty(JObject(properties), name) :: tail =>
      Right(JObject(properties :+ (name -> JString(string))) :: tail)
    case _ => Left("unexpected string: %s, stack: %s".format(string, stack))
  }

  private def jValue(
      stack: List[JValue],
      jValue: JValue
  ): Either[String, List[JValue]] = stack match {
    case JProperty(JObject(properties), name) :: tail =>
      Right(JObject(properties :+ (name -> jValue)) :: tail)
    case JArray(elements) :: tail => Right(JArray(elements :+ jValue) :: tail)
    case _ :: _ =>
      Left("unexpected property value: %s, stack: %s".format(jValue, stack))
    case Nil => Right(List(jValue))
  }
}
