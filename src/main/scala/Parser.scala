sealed abstract class JValue
case object JNull extends JValue
case class JArray(elements: List[JValue]) extends JValue
case class JObject(properties: Map[String, JValue]) extends JValue
case class JString(value: String) extends JValue
case class JNumber(value: Double) extends JValue
case class JBool(value: Boolean) extends JValue
private case class JProperty(name: String) extends JValue

object Parser {
  import Lexer._

  val JTrue = JBool(true)
  val JFalse = JBool(false)

  type Result = Either[String, JValue]

  def parse(json: String): Result = Lexer.tokenize(json) match {
    case Left(error) => Left(error)
    case Right(Tuple2(tokens, remaining)) =>
      val stack = tokens.foldLeft(List.empty[JValue]) { (stack, token) =>
        println("TOKEN: %s".format(token))
        token match {
          case CommaToken(_) => stack
          case OpenSquareBracketToken(_) =>
            push(stack, JArray(List.empty[JValue]))
          case CloseSquareBracketToken(_) =>
            pop(stack) match {
              case Right(Tuple2(popped, newStack)) =>
                jValue(newStack, popped).toOption.get
              case _ => throw new Exception("unexpected ]: %s".format(stack))
            }
          case OpenCurlyBraceToken(_) =>
            push(stack, JObject(Map.empty[String, JValue]))
          case CloseCurlyBraceToken(_) =>
            pop(stack) match {
              case Right(Tuple2(popped, newStack)) =>
                jValue(newStack, popped).toOption.get
              case _ => throw new Exception("unexpected ]: %s".format(stack))
            }
          case ColonToken(_) => colon(stack).toOption.get
          case StringToken(value: String, _) =>
            string(stack, value).toOption.get
          case BoolTrueToken(_)  => jValue(stack, JTrue).toOption.get
          case BoolFalseToken(_) => jValue(stack, JFalse).toOption.get
          case NumberToken(number: Double, _) =>
            jValue(stack, JNumber(number)).toOption.get
          case _ => stack
        }
      }
      stack match {
        case result :: Nil => Right(result)
        case _ => Left("malformed JSON document: %s".format(stack))
      }
  }

  private def push(stack: List[JValue], jValue: JValue): List[JValue] =
    jValue :: stack

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
      case JProperty(name) :: _ => Right(stack)
      case _                    => Left("unexpected colon: %s".format(stack))
    }

  private def string(
      stack: List[JValue],
      string: String
  ): Either[String, List[JValue]] = stack match {
    case JArray(elements) :: tail =>
      Right(JArray(elements :+ JString(string)) :: tail)
    case JObject(properties) :: tail => Right(JProperty(string) :: tail)
    case JProperty(name) :: JObject(elements) :: tail =>
      Right(JObject(elements + (name -> JString(string))) :: tail)
    case _ => Left("unexpected string: %s, stack: %s".format(string, stack))
  }

  private def jValue(
      stack: List[JValue],
      jValue: JValue
  ): Either[String, List[JValue]] = stack match {
    case JProperty(name) :: JObject(properties) :: tail =>
      Right(JObject(properties + (name -> jValue)) :: tail)
    case JArray(elements) :: tail => Right(JArray(elements :+ jValue) :: tail)
    case _ :: _ =>
      val msg = "unexpected property value: %s, stack: %s".format(jValue, stack)
      println("ERROR MSG: %s".format(msg))
      Left(msg)
    case Nil => Right(List(jValue))
  }
}
