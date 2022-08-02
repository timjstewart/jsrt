sealed case class Position(line: Int, column: Int) {
  def moveByChar(char: Char): Position = char match {
    case '\n' => this.copy(line = line + 1, column = 0)
    case '\r' => this.copy(column = 0)
    case _    => this.copy(column = column + 1)
  }
}

sealed abstract class Token(value: String, pos: Position)
case class NEWLINE(pos: Position) extends Token("\n", pos)
case class QUOTE(pos: Position) extends Token("\"", pos)
case class COMMA(pos: Position) extends Token(",", pos)
case class OPEN_SQUARE_BRACKET(pos: Position) extends Token("[", pos)
case class CLOSE_SQUARE_BRACKET(pos: Position) extends Token("]", pos)
case class OPEN_CURLY_BRACE(pos: Position) extends Token("{", pos)
case class CLOSE_CURLY_BRACE(pos: Position) extends Token("}", pos)
case class COLON(pos: Position) extends Token(":", pos)
case class CHARACTER(value: Character, pos: Position)
    extends Token(value.toString, pos)
case class STRING(value: String, pos: Position) extends Token(value, pos)
case class BOOL_TRUE(pos: Position) extends Token("true", pos)
case class BOOL_FALSE(pos: Position) extends Token("false", pos)
case class NUMBER(value: Double, pos: Position) extends Token(value.toString, pos)

case class EOF(pos: Position) extends Token("EOF", pos)

class LexerError(message: String) extends Exception(message)

object Lexer {

  import scala.io.Source

  def tokenize(input: Source): Either[String, List[Token]] = {
    var curPos = Position(1, 0)
    try {
      Right(input.map { c =>
        c match {
          case '\n' =>
            val newLinePos = curPos.copy(column = curPos.column + 1)
            curPos = curPos.moveByChar(c)
            NEWLINE(newLinePos)
          case _ =>
            curPos = curPos.moveByChar(c)
            c match {
              case '"' =>
                val origPos = curPos.copy()
                val (string, newPos) = tokenizeString(input, curPos)
                curPos = newPos
                STRING(string, origPos)
              case ',' => COMMA(curPos)
              case '[' => OPEN_SQUARE_BRACKET(curPos)
              case ']' => CLOSE_SQUARE_BRACKET(curPos)
              case '{' => OPEN_CURLY_BRACE(curPos)
              case '}' => CLOSE_CURLY_BRACE(curPos)
              case ':' => COLON(curPos)
              case 't' =>
                val origPos = curPos.copy()
                tokenizeBool(input, c, curPos, "true") match {
                  case Right(Tuple2(token, newPos)) =>
                    curPos = newPos
                    token
                  case Left(error) => throw new LexerError(error)
                }
              case 'f' =>
                val origPos = curPos.copy()
                tokenizeBool(input, c, curPos, "false") match {
                  case Right(Tuple2(token, newPos)) =>
                    curPos = newPos
                    token
                  case Left(error) => throw new LexerError(error)
                }
              case _ => CHARACTER(c, curPos)
            }
        }
      }.toList :+ EOF(curPos.copy(column = curPos.column + 1)))
    } catch {
      case ex: LexerError => Left(ex.getMessage)
    }
  }

  private def tokenizeString(
      input: Iterator[Char],
      startPos: Position
  ): Tuple2[String, Position] = {
    val result = new StringBuffer()
    var curPos = startPos.copy()
    var done = false
    var escaped = false
    do {
      if (input.hasNext) {
        val c = input.next()
        curPos = curPos.moveByChar(c)

        if (c == '"' && !escaped) {
          done = true
        } else {
          result.append(c)
        }

        escaped = false

        if (c == '\\') {
          escaped = true
        }

      } else {
        done = true
      }
    } while (input.hasNext && !done)
    (result.toString, curPos)
  }

  private def tokenizeBool(
      input: Iterator[Char],
      firstChar: Char,
      startPos: Position,
      literal: String
  ): Either[String, Tuple2[Token, Position]] = {
    require(firstChar == literal(0))
    var pos = startPos.copy()
    try {
      for (char <- literal.drop(1)) {
        val readChar = input.next()
        pos = pos.moveByChar(readChar)
        if (readChar != char) {
          throw new Exception(
            "mismatch: expected: %s, read: %s".format(char, readChar)
          )
        }
      }
      literal match {
        case "true"  => Right(BOOL_TRUE(startPos), pos)
        case "false" => Right(BOOL_FALSE(startPos), pos)
        case _       => Left("failed to tokenize boolean")
      }
    } catch {
      case ex: Exception =>
        println("EX: %s".format(ex.getMessage()))
        Left(ex.getMessage())
    }
  }
}
